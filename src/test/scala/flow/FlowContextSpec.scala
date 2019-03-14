package flow

import org.scalatest._

class FlowContextSpec extends FlatSpec with Matchers {

  "FlowContext" should "solve the single self-referential" +
    " accumulator example" in {

    val ctx = FlowContext.empty
    import ctx._

    lazy val a: Flow[Double] = accumulator(delay(a), 1.0) {
      (x, y) => (x + 1764 / y) / 2
    }

    a.get should equal(42.0)
    a.get should equal(42.0) // repetition intentional, branch coverage!
  }

  "delay" should "not influence the result of `.get`" in {
    val ctx = FlowContext.empty
    import ctx._

    lazy val a: Flow[Double] = delay {
      accumulator(a, 1.0) {
        (x, y) => (x + 1764 / y) / 2
      }
    }

    a.get should equal(42.0)
    a.get should equal(42.0) // repetition intentional, branch coverage!
  }

  "Computation of initial value from pure values" should "work" in {
    val ctx = FlowContext.empty
    import ctx._

    val a = pure(true)
    val b = pure(true)
    val c = accumulator(map2(a, b)(_ && _), false)(_ || _)

    c.get should be(true)
  }

  "basic combinators" should "work" in {
    val ctx = FlowContext.empty
    import ctx._

    lazy val a: Flow[Double] = accumulator(delay{
      val div = a.map(1764.0 / _)
      val pair = a zip div
      val sum = pair.map{ case (x, y) => x + y }
      val avg = sum.map(_ / 2.0)
      avg
    }, 1.0){
      (_, latest) => latest
    }

    a.get should equal(42.0)
  }

  "pure, ap, ap2" should "work" in {
    val ctx = FlowContext.empty
    import ctx._

    lazy val a: Flow[Double] = accumulator(delay{
      val nDiv = pure{ (a: Double) => 1764.0 / a }
      val nDivA = ap(nDiv)(a)

      val avg = pure{ (a: Double, b: Double) => (a + b) / 2.0 }
      ap2(avg)(a, nDivA)
    }, 1.0){
      (_, latest) => latest
    }

    a.get should equal(42.0)
  }

  "map2" should "work" in {
    val ctx = FlowContext.empty
    import ctx._

    lazy val a: Flow[Double] = accumulator(delay{
      val nDivA = a.map(1764.0 / _)
      map2(a, nDivA)( (x, y) => (x + y) / 2.0)
    }, 1.0){
      (_, latest) => latest
    }

    a.get should equal(42.0)
  }

  "zip3, unit" should "work" in {
    val ctx = FlowContext.empty
    import ctx._

    val two = unit.map(_ => 2.0)
    lazy val a: Flow[Double] = accumulator(delay{
      val nDivA = a.map(1764.0 / _)
      val triple = a.zip3(nDivA, two)
      triple.map{ case (x, y, t) => (x + y) / t }
    }, 1.0){
      (_, latest) => latest
    }

    a.get should equal(42.0)
    a.get should equal(42.0) // repetition intentional, branch coverage!
  }

  "all formal combinators" should "get the final results" in {
    val ctx = FlowContext.empty
    import ctx._
    def new42 = {
      lazy val a: Flow[Double] = accumulator(delay(a), 1.0) {
        (x, y) => (x + 1764 / y) / 2
      }
      a
    }
    unit.get should equal( () )
    pure(42.0).get should equal(42.0)
    unit.zip(new42).get._2 should equal(42.0)
    new42.map(_ * 2).get should equal(84.0)
    (ap2(pure((x: Double, y: Double) => x + y))(new42, new42).get
      should equal(84.0)
    )
    (map2(new42, new42){(x: Double, y: Double) => x + y }.get
      should equal(84.0)
    )
  }

  "0-dependency accumulator" should "keep initial constant value" in {
    val ctx = FlowContext.empty
    import ctx._
    val a = accumulator[Int, Set[Int]](
      Set.empty[Flow[Int]],
      Set(1, 2, 3)
    )(_ ++ _)

    a.get should equal(Set(1, 2, 3))
  }

  "sequence & traverse" should "satisfy a few important equations" in {
    val ctx = FlowContext.empty
    import ctx._
    def dummy(withResult: Double) = {
      val square = withResult * withResult
      lazy val a: Flow[Double] = accumulator(delay(a), 1.0) {
        (x, y) => (x + square / y) / 2
      }
      a
    }

    val inputs = (10 to 20).map(_.toDouble).toList

    val t = traverse(inputs)(dummy)
    val s = sequence(inputs.map(dummy))

    s.get should equal(t.get)

    val dummyInputs = inputs.map(dummy)

    val tId = traverse(dummyInputs)(identity)
    val sAgain = sequence(dummyInputs)

    sAgain.get should equal(tId.get)
  }
}
