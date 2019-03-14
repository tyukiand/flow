package flow.impl

import flow.{FlowContext}

/** The default implementation of `FlowContext`.
  *
  * Ties together `Worklist`, `Dfs`, `ReactiveCell`.
  * Works sequentially for now.
  */
private[flow] class FlowContextImpl extends FlowContext {

  /** A `Flow[A]` is either a `FlowAccumulator` that holds
    * mutable state of type `A`, or a formal element
    * of type `Flow[A]` that holds no state,
    * but instead computes its current value
    * from values of other elements.
    */
  trait Flow[A] extends FlowContext.Flow[A, Flow] {

    /** Upstream accumulators on which values of
      * this `Flow` depend directly (without
      * transitive dependencies).
      */
    private[FlowContextImpl]
    def upstreamAccumulators: Set[FlowAccumulator[_, _]]

    /** Value of the cell used during iteration */
    private[FlowContextImpl]
    def currentFlowValue: A

    /** Final converged solution */
    def get: A

    def map[B](f: A => B): Flow[B] = new FlowMapper[A, B](this, f)
    def zip[B](b: Flow[B]): Flow[(A, B)] = new FlowZip(this, b)

    // $COVERAGE-OFF$
    private var tracingName: String = super.toString
    private[flow] def withTracingName(n: String): this.type = {
      tracingName = n
      this
    }

    override def toString = tracingName
    // $COVERAGE-ON$
  }

  /* Factory method for accumulators, redirects to `FlowAccumulator` */
  def accumulator[A, B](
    accumulatedInputs: Set[Flow[A]],
    initialValue: B
  )(
    combineOldValueWithChanges: (B, Set[A]) => B
  ): Flow[B] = new FlowAccumulator(
    accumulatedInputs,
    initialValue,
    combineOldValueWithChanges
  )



  /** Depth-first search time, helps keep the time markers consistent
    * across multiple launches of the depth-first-search.
    *
    * The depth-first-search can be launched only in the `get` method
    * of a `FlowAccumulator[_]`
    */
  private implicit val dfsTime = Dfs.newTime

  /** Shortcut for the set of changed inputs of a `FlowAccumulator` */
  private type ChangedInputs = Set[Flow[_]]

  /** A worklist that holds `FlowAccumulators` sorted in the order
    * of their DFS finishing times. For each `FlowAccumulator`
    * currently in the list, there is a set `ChangedInputs` (`Set[Flow[_]]`)
    * of inputs that have been changed recently, and at which the
    * `FlowAccumulator` should take a look while updating its value.
    */
  private val worklist = Worklist.empty[FlowAccumulator[_, _], ChangedInputs](
    (acc: FlowAccumulator[_, _], changedInputs: ChangedInputs) => {
      acc.update(changedInputs)
    },
    Set.empty,
    _ ++ _
  )(Dfs.finishTimeOrdering[FlowAccumulator[_, _]])

  /** The central workhorse of the whole networks.
    *
    * Holds mutable state (a value of type `B`) that is improved
    * during the iterations.
    *
    * Acts as a node of the Depth-first-search, with other
    * upstream `FlowAccumulator`s being the child nodes.
    *
    * Computes its current value from the `accumulatedInputs`.
    *
    * Note that the direction of the information flow from
    * the upstream `FlowAccumulators` through the `accumulatedInputs`
    * to this node is opposite to the direction of the edges
    * using for DFS.
    */
  private class FlowAccumulator[A, B](
    private[FlowContextImpl] val accumulatedInputs: Set[Flow[A]],
    initialValue: B,
    combineOldValueWithChanges: (B, Set[A]) => B,
  ) extends ReactiveCell[B, ChangedInputs](initialValue)
    with Dfs.Node[FlowAccumulator[_, _]]
    with Flow[B]
  {

    /** A map that tells us which inputs could change
      * if an upstream `FlowAccumulator` changes its value.
      */
    private[FlowContextImpl]
    lazy val childNodesToInputs
    : Map[FlowAccumulator[_, _], Set[Flow[A]]] = {
      import collection.mutable.{HashMap, HashSet}

      val hm = HashMap.empty[FlowAccumulator[_, _], HashSet[Flow[A]]]

      for {
        i <- accumulatedInputs
        uAcc <- i.upstreamAccumulators
      } {
        // don't do this:
        // hm(uAcc) += i
        // see also: https://stackoverflow.com/questions/52099989
        hm.getOrElseUpdate(uAcc, HashSet.empty[Flow[A]]) += i
      }

      val res = hm.mapValues(_.toSet).toMap

      res
    }

    /** Upstream accumulators (no transitive dependencies,
      * loops possible, so "upstream" is not strict).
      */
    def childNodes: Set[FlowAccumulator[_, _]] = childNodesToInputs.keySet

    /** Cuts off all transitive dependencies. For all
      * `Flow`s downstream from this node, the only
      * visible upstream `FlowAccumulator` is this accumulator.
      */
    private[FlowContextImpl]
    def upstreamAccumulators = Set(this)

    /** Keeping the mutable value is handled by `ReactiveCell`,
      * from which this class inherits.
      */
    private[FlowContextImpl]
    def currentFlowValue = /* ReactiveCell */currentCellValue

    private var isInitialized = false

    /** Recomputes the value from its own mutable state and
      * the current values of the inputs that changed.
      */
    def recomputeValue(changedInputs: ChangedInputs): B = {

      // $COVERAGE-OFF$
      assert(!changedInputs.isEmpty,
        "If no values were changed, then there should be nothing to " +
        "recompute, but `recomputeValue` invoked anyway. " +
        "Must be a bug in the `flow` package, sorry about that. " +
        "Please submit a bug report. Assert-ID: TSNH-0004"
      )
      // $COVERAGE-ON$

      combineOldValueWithChanges(
        this.currentFlowValue,
        changedInputs.map(_.currentFlowValue.asInstanceOf[A])
      )
      // Note: `asInstanceOf` seems unavoidable here, because
      // we are getting these `changedInputs`-values from a
      // worklist, and the worklist cannot know all the types
      // involved in all the calculations in the entire graph,
      // so it has to store the `ChangedInputs` without type
      // parameters.
    }

    /** Just "gets" the solution.
      *
      * To get the final solution, quite a few steps are necessary.
      * All upstream dependencies relevant in the current optimization
      * phase must be discovered. The accumulators must be
      * interconnected by registering them as observers of each other.
      * Discovery and finish times of the DFS must be computed to
      * define a good order on the nodes. Finally, all discovered nodes
      * must be put into worklist to start the actual computation.
      * Once the iteration terminates, the final can be returned. This
      * is the result of the `get`.
      */
    def get: B = {
      if (!this.isDiscovered) {

        // $COVERAGE-OFF$
        assert(!this.isFinished,
          "A node has been finished before it has been discovered. " +
          "Must be a bug in the framework. Please report this bug. " +
          "Assert-ID: TSNH-0002."
        )
        // $COVERAGE-ON$

        discoverAndSolveStartingFrom(this)

      } else {
        /* This node has already been discovered,
         * the solution must already have converged,
         * so we can just return the `currentCellValue`.
         */
      }
      currentCellValue
    }
  }

  // discover all transitive dependencies,      (dfs does this)
  //
  // make sure that the freshly discovered      v
  // accumulators listen to changes of their    v
  // child nodes,                               (registerOnUpdate)
  //
  // put every node that is done with dfs into  v
  // worklist                                   (worklist.addTodos)
  //
  // run worklist to convergence                (workUntilEmpty)
  private def discoverAndSolveStartingFrom(a: FlowAccumulator[_, _]): Unit = {

    Dfs[FlowAccumulator[_, _]](
      a,
      n => {
        // on discovery: register as observer
        for (upstreamAcc <- n.childNodes) {
          upstreamAcc.registerOnUpdate {
            val addedTodos =
              n.childNodesToInputs(upstreamAcc).asInstanceOf[Set[Flow[_]]]

            // $COVERAGE-OFF$
            assert(!addedTodos.isEmpty,
              "An upstream accumulator returned empty set of inputs. " +
              "How can it be 'upstream' then in the first place, " +
              "if there are no inputs coming from it? Must be a bug in " +
              "`flow`. Please submit a bug report. Assert-ID: TSNH-0005"
            )
            // $COVERAGE-ON$

            worklist.addTodos(n, addedTodos)
          }
        }
      },
      n => {

        val allInputs = n.accumulatedInputs.map(_.asInstanceOf[Flow[_]])
        if (allInputs.isEmpty) {
          /* do nothing, keep initial value */
          // note: in general, an accumulator is not forced to
          // have any inputs at all, it can be a constant value too.
        } else {
          worklist.addTodos(n, allInputs)
        }
      }
    )

    worklist.workUntilEmpty()
  }

  /** Implements `map`. Formal operation, does not hold state. */
  private class FlowMapper[A, B](input: Flow[A], f: A => B) extends Flow[B] {
    private[FlowContextImpl]
    lazy val upstreamAccumulators = input.upstreamAccumulators

    private[FlowContextImpl]
    def currentFlowValue = f(input.currentFlowValue)

    def get = f(input.get)
  }

  /** Implements `zip`. Formal operation, does not hold state. */
  private class FlowZip[A, B](a: Flow[A], b: Flow[B]) extends Flow[(A, B)] {
    private[FlowContextImpl]
    lazy val upstreamAccumulators =
      a.upstreamAccumulators ++ b.upstreamAccumulators

    private[FlowContextImpl]
    def currentFlowValue = (a.currentFlowValue, b.currentFlowValue)

    def get = (a.get, b.get)
  }

  /** A `Flow[A]` that holds no state, but simply always returns
    * the same constant value.
    */
  private class ConstantFlow[A](val constantValue: A) extends Flow[A] {
    def get: A = constantValue
    private[FlowContextImpl] def upstreamAccumulators = Set.empty
    private[FlowContextImpl] def currentFlowValue = constantValue
  }

  def pure[A](a: A): Flow[A] = new ConstantFlow(a)

  def delay[A](f: => Flow[A]): Flow[A] = new Flow[A] {
    lazy val delayedF = f

    private[FlowContextImpl]
    lazy val upstreamAccumulators = delayedF.upstreamAccumulators

    private[FlowContextImpl]
    def currentFlowValue = delayedF.currentFlowValue

    def get = delayedF.get

    // $COVERAGE-OFF$
    override def toString = delayedF.toString
    // $COVERAGE-ON$
  }

}