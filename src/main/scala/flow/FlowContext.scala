package flow

import scala.language.higherKinds

/** Entry point of the `flow` package.
  *
  * Provides a type constructor `Flow[_]` and
  * several combinators that can be used to
  * build values of type `Flow[_]`.
  *
  * Once all the flow equations are specified,
  * the values of type `Flow[X]` can be eliminated
  * by invoking the method `.get` on them, which
  * will return a solution of type `X`.
  */
trait FlowContext {
  type Flow[A] <: FlowContext.Flow[A, Flow]

  /** Accumulator with multiple input values.
    *
    * Updates its current value by combining
    * it with new changes of only those inputs
    * that actually changed.
    */
  // (yes, the separate argument list is necessary for proper type inference)
  def accumulator[A, B](
    accumulatedInputs: Set[Flow[A]],
    initialValue: B
  )(
    combineOldValueWithChanges: (B, Set[A]) => B
  ): Flow[B]

  /** Creates an accumulator node
    * that accumulates the `accumulated` flow
    * using `combineOldNew` function.
    */
  // (yes, the separate argument list is necessary for proper type inference)
  def accumulator[A, B](
    accumulated: Flow[A],
    initialValue: B
  )(
    combineOldNew: (B, A) => B
  ): Flow[B] = accumulator(
    Set(accumulated),
    initialValue
  )(
    (b, changedAs) => {
      // $COVERAGE-OFF$
      assert(
        changedAs.size == 1,
        "Accumulator with a single input was notified of a " +
        s"simultaneous change of ${changedAs.size} != 1 inputs. " +
        "This must be a bug in the `flow` package itself, it " +
        "should never happen. If you ever encounter this " +
        "error, please submit a bug report. Assert-ID: TSNH-0001"
      )
      // $COVERAGE-ON$

      // changedAs.foldLeft(b)(combineOldNew) // don't delete yet
      combineOldNew(b, changedAs.head)
    }
  )


  /** Helper method for breaking occasional circles. */
  def delay[A](a: => Flow[A]): Flow[A]

  // Selected pieces of the `Applicative[_]` interface, to make it
  // somewhat useful without including Scalaz/Cats compatibility
  // packages. *I know it's a little bit of a duplicate effort,
  // don't force the users to use those libraries if they don't
  // need them.*

  /** Creates a flow which always has the constant value `a`. */
  def pure[A](a: A): Flow[A]
  def unit: Flow[Unit] = pure( () )

  def map2[A, B, C](a: Flow[A], b: Flow[B])(f: (A, B) => C): Flow[C] = {
    (a zip b) map { x => f(x._1, x._2) }
  }

  def ap[A, B](f: Flow[A => B])(x: Flow[A]): Flow[B] = {
    (f zip x).map{ case (func, arg) => func(arg) }
  }

  def ap2[A, B, C](f: Flow[(A, B) => C])(a: Flow[A], b: Flow[B]): Flow[C] = {
   f.zip3(a, b).map{ case (func, x, y) => func(x, y) }
  }

  // Poor man's `Traverse[_]`. Included here for the same reason as
  // the parts of `Applicative[_]` interface.

  // TODO: think about a more general `sequence` implementation later.
  // import collection.TraversableOnce
  // import collection.generic.CanBuildFrom

  def sequence[A](list: List[Flow[A]]): Flow[List[A]] = list match {
    // Note: didn't immediately understand how to make it work
    // with a CBF and `TraversableOnce`, throwing mutable builders
    // into the mix seemed weird. Constrained it to `List` for now.
    case Nil => pure(Nil)
    case h :: t => map2(h, sequence(t))(_ :: _)
  }

  def traverse[A, B](list: List[A])(f: A => Flow[B]): Flow[List[B]] = {
    list match {
      case Nil => pure(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }
  }

}

object FlowContext {
  trait Flow[A, F[X] <: Flow[X, F]] {
    def get: A
    def map[B](f: A => B): F[B]
    def zip[B](other: F[B]): F[(A, B)]

    def zip3[B, C](b: F[B], c: F[C]): F[(A, B, C)] = (this zip b zip c).map {
      case ((x, y), z) => (x, y, z)
    }

    private[flow] def withTracingName(n: String): this.type
  }

  /** Creates a new empty `FlowContext`, without any `Flow`-nodes
    * added to it.
    */
  def empty: FlowContext = new impl.FlowContextImpl
}