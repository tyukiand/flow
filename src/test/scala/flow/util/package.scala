package flow

/** Memoization functions used in the `Grammar` example;
  * 
  * Not in any way critical to the framework itself, or even to the majority
  * of tests.
  */
package object util {

  def memoize[A, B](fGenerator: (A => B) => (A => B)): (A => B) = {
    val hm = collection.mutable.HashMap.empty[A, B]
    lazy val fMem: (A => B) = (a: A) => {
      hm.getOrElseUpdate(a, fGenerator(fMem)(a))
    }
    fMem
  }

  def memoize[A, B, C, D](
    fGen: (A => B, C => D) => (A => B),
    gGen: (A => B, C => D) => (C => D)
  ): (A => B, C => D) = {
    val fhm = collection.mutable.HashMap.empty[A, B]
    val ghm = collection.mutable.HashMap.empty[C, D]
    lazy val fMem: (A => B) = a => {
      fhm.getOrElseUpdate(a, fGen(fMem, gMem)(a))
    }
    lazy val gMem: (C => D) = c => {
      ghm.getOrElseUpdate(c, gGen(fMem, gMem)(c))
    }
    (fMem, gMem)
  }

}