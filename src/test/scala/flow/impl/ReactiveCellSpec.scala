package flow.impl

import org.scalatest._

class ReactiveCellSpec extends FlatSpec with Matchers {

  "ReactiveCell" should "notify observers" in {

    //                     C
    //                    / \
    //                   A   B
    //
    // A and B vary.

    var aValue = 42
    var bValue = 58

    val a = new ReactiveCell[Int, Unit](0) {
      def recomputeValue(u: Unit) = aValue
    }

    val b = new ReactiveCell[Int, Unit](0) {
      def recomputeValue(u: Unit) = bValue
    }

    val c = new ReactiveCell[Int, Unit](0) {
      def recomputeValue(u: Unit) = {
        a.currentCellValue + b.currentCellValue
      }
    }

    a.registerOnUpdate{ c.update( () ) }
    b.registerOnUpdate{ c.update( () ) }

    a.update( () )
    b.update( () )


    c.currentCellValue should equal(100)

    aValue = 142; a.update( () )

    c.currentCellValue should equal(200)

    bValue = -142; b.update( () )

    c.currentCellValue should equal(0)
  }

  it should "not notify observers if nothing changes" in {
    //       C
    //       |
    //       B
    //       |
    //       A
    //
    // A changes twice, B changes once, C should also
    // change only once, not twice.

    var aValue = 42
    val a = new ReactiveCell[Int, Unit](0) {
      def recomputeValue(u: Unit) = aValue
    }

    val b = new ReactiveCell[Boolean, Unit](false) {
      def recomputeValue(u: Unit) = a.currentCellValue > 50
    }

    var numCUpdates = 0
    val c = new ReactiveCell[Int, Unit](-777) {
      def recomputeValue(u: Unit) = {
        numCUpdates += 1
        if (b.currentCellValue) 1000
        else -1000
      }
    }

    a.registerOnUpdate{ b.update( () ) }
    b.registerOnUpdate{ c.update( () ) }

    a.update( () )

    a.currentCellValue should equal(42)
    b.currentCellValue should equal(false)
    c.currentCellValue should equal(-777)

    numCUpdates should equal(0)

    aValue = 58; a.update( () )

    a.currentCellValue should equal(58)
    b.currentCellValue should equal(true)
    c.currentCellValue should equal(+1000)

    numCUpdates should equal(1)

    aValue = 99; a.update( () )

    a.currentCellValue should equal(99)
    b.currentCellValue should equal(true)
    c.currentCellValue should equal(+1000)

    numCUpdates should equal(1)
  }

}
