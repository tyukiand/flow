package flow.impl

import org.scalatest._

class WorklistSpec extends FlatSpec with Matchers {

  "Worklist" should "store strings and add integers" in {

    var result: List[(String, Int)] = Nil

    val w = Worklist.empty[String, Int](result :+= (_, _), 0, _ + _)

    w.addTodos("b", 42)
    w.addTodos("a", 3)
    w.addTodos("c", 400)
    w.addTodos("a", 7)
    w.addTodos("b", 58)
    w.addTodos("c", 600)

    w.workUntilEmpty()

    result should equal(List(
      "a" -> 10,
      "b" -> 100,
      "c" -> 1000
    ))
  }

  /* Should just not throw anything */
  it should "just exit immediately when `workUntilEmpty()` called on empty" in {
    Worklist.empty[Int, Long]((_, _) => {}, 0L, _ + _).workUntilEmpty()
  }

  /* A really dumb way to factorize numbers */
  it should "work on tiny prime-number example" in {

    val rnd = new util.Random(42)
    for (n <- 1 to 50) {
      val x = rnd.nextInt(1000000) + 11

      // Entries: (what to factorize, how often it repeats)
      var factors: List[Int] = Nil
      def findOneFactor(x: Int): Option[Int] = {
        for (i <- 2 until x) {
          if (x % i == 0) return Some(i)
        }
        None
      }
      lazy val w: Worklist[Int, Int] = Worklist.empty[Int, Int](
        (x, numRepeats) => {
          findOneFactor(x) match {
            case None => factors :::= List.fill(numRepeats)(x)
            case Some(a) => {
              val b = x / a
              w.addTodos(a, numRepeats)
              w.addTodos(b, numRepeats)
            }
          }
        },
        0,
        _ + _
      )(implicitly[math.Ordering[Int]].reverse) // factor big numbers first
                                                // for more fun!

      w.addTodos(x, 1)
      w.workUntilEmpty()

      factors.foldLeft(1)(_ * _) should equal(x)

      // println(s"Factors of $x = " + factors.mkString(" * ")) // neat...
    }
  }
}