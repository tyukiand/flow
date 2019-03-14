package flow.impl

import scala.language.higherKinds

/** Interface of a mutable worklist.
  *
  * The assumption is that there are several
  * "places" (loci) at which some work is to be
  * done. For each locus, a collection of todo-`Todos`s
  * is kept by the worklist. The loci are sorted,
  * so that if there is work to do at multiple loci,
  * the tasks of the locus that is "smaller"  (w.r.t ordering)
  * will be done before the tasks of the locus that is
  * "larger" (w.r.t. ordering).
  */
private[flow] trait Worklist[Locus, Todos] {
  def addTodos(where: Locus, what: Todos): Unit
  def workUntilEmpty(): Unit
}

/** Trait for companion objects of `Worklist` implementations.
  *
  * Provides a factory method for empty worklists.
  */
private[flow] trait WorklistCompanion[W[X, Y] <: Worklist[X, Y]] {
  def empty[Locus, Todos](
    doWork: (Locus, Todos) => Unit,
    emptyAccumulator: => Todos,
    combine: (Todos, Todos) => Todos
  )(
    implicit locusOrdering: math.Ordering[Locus]
  ): W[Locus, Todos]
}

private[flow] object Worklist extends WorklistCompanion[Worklist] {
  def empty[Locus, Todos](
    doWork: (Locus, Todos) => Unit,
    emptyAccumulator: => Todos,
    combine: (Todos, Todos) => Todos
  )(
    implicit locusOrdering: math.Ordering[Locus]
  ): Worklist[Locus, Todos] = SequentialWorklist.empty(
    doWork,
    emptyAccumulator,
    combine
  )
}

private[impl] class SequentialWorklist[Locus, Todos](
  doWork: (Locus, Todos) => Unit,
  emptyAccumulator: => Todos,
  combine: (Todos, Todos) => Todos,
  ord: Ordering[Locus]
) extends Worklist[Locus, Todos] {
  import collection.mutable.{SortedSet, HashMap}

  private var worklist = SortedSet.empty[Locus](ord)

  private val accumulatedTodos = HashMap.empty[Locus, Todos]

  def addTodos(where: Locus, todos: Todos): Unit = {
    worklist += where
    val acc = accumulatedTodos.getOrElseUpdate(where, emptyAccumulator)
    val newAcc = combine(todos, acc)
    accumulatedTodos(where) = newAcc
  }

  def workUntilEmpty(): Unit = {
    while (!worklist.isEmpty) {
      val h = worklist.head
      worklist -= h
      val tasks = accumulatedTodos(h)
      accumulatedTodos -= h

      // crucial: invoke `doWork` *after*
      // removing the head from the
      // worklist (so it can be reinserted
      // for the next iteration)
      doWork(h, tasks)

      // Note: synchronizing this method is
      // insufficient for making it work with
      // a thread of pools in parallel.
      // It won't be a single-line change, needs some thought.
    }
  }
}

private[impl] object SequentialWorklist
extends WorklistCompanion[SequentialWorklist] {

  def empty[Locus, Todos](
    doWork: (Locus, Todos) => Unit,
    emptyAccumulator: => Todos,
    combine: (Todos, Todos) => Todos
  )(
    implicit locusOrdering: math.Ordering[Locus]
  ): SequentialWorklist[Locus, Todos] = new SequentialWorklist(
    doWork,
    emptyAccumulator,
    combine,
    locusOrdering
  )
}

