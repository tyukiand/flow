<BADGES>

## flow

### What is this?

Dataflow algorithm with a simple functional interface.

Allows the user to construct (cyclic) directed graphs of mutually interdependent values in declarative fashion, and then simply query the results of a converging fixpoint iteration.

### Where is this useful?

Mostly for dataflow algorithms on bounded semilattices as they are often found in parser generators and compilers, e.g. for analyzing grammars, type inference, liveness analysis etc.

### A minimalistic example

The following minimal example implements Heron's method for computing square root of `1764`:

```scala
// initialize empty flow context, import combinators
val ctx = FlowContext.empty
import ctx._

// define system of equations with mutually dependent variables
lazy val a: Flow[Double] = accumulator(delay(a), 1.0) {
  (x, y) => (x + 1764 / y) / 2
}

// just get the result
println(a.get) // prints `42.0`
```

It uses numeric values instead of the usual posets, but the principle is the same:

  1. We describe a system of equations (or rather, inequalities) with mutually recursive values (in this case, `a` depends on itself)
  2. We ask what the solution is, and the algorithm gives us the solution.

### A more realistic example

Here is an example that shows how we can compute nullability of non-terminal symbols for a context free grammar with left recursion (which would blow up if we attempted to use naive recursion):

```scala
/** Provides few simple combinators for describing grammars in a style
  * similar to parser combinators. Implements nullablity check using `flow`.
  */
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

// concrete example
object G1 extends Grammar {
  lazy val s: Lang = named("S"){ s ~ a | b }
  lazy val a: Lang = named("A"){ just('a').? | c }
  lazy val b: Lang = named("B"){ just('b').? }
  lazy val c: Lang = named("C"){ just('c') | just('C') }
}

import G1._

assert(isNullable(s))
assert(isNullable(a))
assert(isNullable(b))
assert(!isNullable(c))
```

The full example with the grammar is included as a test case in `test/scala`.

### An observation

Sometimes, the constraints have much better compositionality properties than the solutions:

  * The fixed point (the result of converging iteration) does not have any good compositional properties at all, e.g. you can't define it by a top-down recursion as if it were some kind of "semantics" for some formulas.
  * The constraints (system of inequalities) are nicely composable (essentially an `Applicative` functor).
