package flow.impl

/** Minimalistic implementation of a
  * depth-first search on a graph of
  * nodes that save their discovery
  * and dfs-finish times.
  *
  * The only advantage of this
  * DFS implementation is that it takes a
  * `Time` object when starting to explore
  * a node. In this way, we don't have to
  * enumerate all the nodes up front, we can
  * discover them on-demand, but keep the
  * discovery and finish times consistent
  * between multiple launches of the DFS from
  * multiple "roots".
  */
private[impl] object Dfs {

  /** A node that can participate in a depth-first-search.
    * It knows what its discovery and finish times are,
    * and it knows where to find its child nodes.
    */
  trait Node[N <: Node[N]] {
    self: N =>

    var discoveryTime: Long = -1
    var finishTime: Long = -1
    def isDiscovered = discoveryTime >= 0
    def isFinished = finishTime >= 0

    def childNodes: Set[N]
  }

  /** Orders `Node`s by increasing
    * finish time.
    */
  def finishTimeOrdering[N <: Node[N]] =
    math.Ordering.by[N, Long](_.finishTime)

  /** Essentially just an object holding a single
    * mutable `Long` counter.
    */
  class Time private[Dfs]() {
    private var time: Long = 0

    /** Increments the time by `1` */
    def tick(): Unit = {
      time += 1
    }

    /** Gets current DFS time */
    def get: Long = time
  }

  def newTime: Time = new Time()

  /** Performs depth-first search starting at the
    * `currentNode`. Every time it discovers a new
    * node, it invokes `onDiscovery` on it. Every
    * time the exploration of a node and all its
    * children is finished, `onFinish` is called
    * on the node.
    *
    * The discovery and finish time are determined
    * by the implicit `Time`, so this function
    * can be invoked multiple times on nodes that
    * are not reachable from each other, still
    * keeping the discovery and finish times
    * consistent.
    */
  def apply[N <: Node[N]](
    currentNode: N,
    onDiscovery: N => Unit,
    onFinish: N => Unit
  )(implicit time: Time): Unit = {
    // $COVERAGE-OFF$
    assert(!currentNode.isDiscovered,
      "Dfs started on a node that has already been discovered. " +
      "Must be a bug in the framework. Please report a bug. " +
      "Assert-ID: TSNH-0003"
    )
    // $COVERAGE-ON$

    time.tick()
    currentNode.discoveryTime = time.get
    onDiscovery(currentNode)

    for (c <- currentNode.childNodes) {
      if (!c.isDiscovered) {
        Dfs(c, onDiscovery, onFinish)
      }
    }

    time.tick()
    currentNode.finishTime = time.get
    onFinish(currentNode)
  }

}