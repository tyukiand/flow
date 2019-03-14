package flow.impl

import org.scalatest._

class DfsSpec extends FlatSpec with Matchers {

  "Dfs" should "work on Cormen's example" in {

    class N(name: String, cs: => Set[N]) extends Dfs.Node[N] {
      override def toString = name
      def childNodes = cs
    }

    def m(name: String, cs: => Set[N]): N = new N(name, cs)

    lazy val u: N = m("u", Set(v, x))
    lazy val v: N = m("v", Set(y))
    lazy val w: N = m("w", Set(y, z))
    lazy val x: N = m("x", Set(v))
    lazy val y: N = m("y", Set(x))
    lazy val z: N = m("z", Set(z))

    implicit val t = Dfs.newTime

    var log: List[String] = Nil

    val nodes = List(u, v, w, x, y, z)

    for (s <- nodes) {
      if (!s.isDiscovered) {
        Dfs[N](
          s,
          n => log :+= s"Discovered $n",
          n => log :+= s"Finished $n[${n.discoveryTime}, ${n.finishTime}]"
        )
      }
    }

    all(nodes) should (
      have ('isFinished(true)) and
      have ('isDiscovered(true))
    )


    // Carefully compared with Cormen Leiserson Rivest Figure 22.4,
    // reusable for tests
    val expectedLog = List(
      "Discovered u",
      "Discovered v",
      "Discovered y",
      "Discovered x",
      "Finished x[4, 5]",
      "Finished y[3, 6]",
      "Finished v[2, 7]",
      "Finished u[1, 8]",
      "Discovered w",
      "Discovered z",
      "Finished z[10, 11]",
      "Finished w[9, 12]"
    )


    log should equal(expectedLog)

  }

  "Dfs.finishTimeOrdering" should
  "order nodes in ascending order w.r.t finishTime" in {
    sealed trait N extends Dfs.Node[N] {
      def childNodes = Set.empty
    }
    object A extends N
    object B extends N
    A.finishTime = 200
    B.finishTime = 100

    implicit val ord = Dfs.finishTimeOrdering[N]
    import ord._

    (B < A) should be(true)
  }

}