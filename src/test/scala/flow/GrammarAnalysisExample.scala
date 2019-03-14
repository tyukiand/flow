package flow

import org.scalatest._

/** Example: Grammar analysis.
  *
  * A more interesting example that makes use of the framework to 
  * compute something not entirely trivial - performing some simple
  * analysis on a grammar with left-recursion and mutually recursive
  * rules.
  */
class GrammarAnalysisExample extends FlatSpec with Matchers {

  trait Grammar {
    sealed trait Lang {
      def ~ (other: Lang): Lang = Concat(this, other)
      def | (other: Lang): Lang = Choice(List(this, other))
      def ? : Lang = Empty | this
      def * : Lang = Rep(this)
      def + : Lang = this ~ Rep(this)
    }

    case object Empty extends Lang
    case class SingleCharacter(c: Char) extends Lang
    def just(c: Char): Lang = SingleCharacter(c)
    case class Concat(a: Lang, b: Lang) extends Lang
    case class Choice(langs: List[Lang]) extends Lang {
      override def | (other: Lang) = Choice(other :: langs)
    }
    case class Rep(a: Lang) extends Lang

    /** Wrapper for right-hand sides of mutually recursive definitions */
    class Named(val name: String, thunk: () => Lang) extends Lang {
      lazy val instantiated: Lang = thunk()
    }

    def named(name: String)(l: => Lang) = new Named(name, () => l) {
      override def toString = name
    }

    private val flowContext = FlowContext.empty

    def isNullable(l: Lang): Boolean = isNullableFlows(l).get

    import flowContext._
    import flow.util.memoize
    private lazy val isNullableFlows = memoize[Lang, Flow[Boolean]] {
      memF => lang =>
      lang match {
        case Empty => pure(true)
        case SingleCharacter(_) => pure(false)
        case Concat(a, b) => accumulator(
          delay { map2(memF(a), memF(b))(_ && _)}, 
          false
        )(_ || _).withTracingName("Concat(" + a + ", " + b + ")")

        case Choice(langs) => accumulator(
          langs.map(l => delay { memF(l) }).toSet,
          false
        ){ 
          (old, changes) => changes.foldLeft(old)(_ || _)
        }.withTracingName("Choice(" + langs.mkString(",") + ")")

        case Rep(_) => pure(true)
        case n: Named => accumulator(
          delay { memF(n.instantiated) },
          false
        )(_ || _).withTracingName(n.name)
      }
    }
  }

  "Nullability" should "be computed correctly in example G1" in {
    object G1 extends Grammar {
      lazy val s: Lang = named("S"){ s ~ a | b }
      lazy val a: Lang = named("A"){ just('a').? | c }
      lazy val b: Lang = named("B"){ just('b').? }
      lazy val c: Lang = named("C"){ just('c') | just('C') }
    }

    // Rename to `isN` because of weird conflict with `Matchers`
    import G1.{isNullable => isN, s, a => wtfMatchersA, b, c} 

    withClue("s") { isN(s) should be(true) }
    withClue("a") { isN(wtfMatchersA) should be(true) }
    withClue("b") { isN(b) should be(true) }
    withClue("c") { isN(c) should be(false) }
  }
}