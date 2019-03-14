package flow.impl

/** Every instance encapsulates a mutable variable (`currentCellValue`)
  * and a list of observers.
  *
  * Every time the `update` method is called,
  * the value is recomputed (using `currentCellValue` and
  * an optional `changeHint`), and if the recomputed value
  * is different from the previous value, then all observers
  * are notified.
  */
private[flow] abstract class ReactiveCell[A, ChangeHint](initialValue: A) {
  private var _currentCellValue = initialValue
  def currentCellValue: A = _currentCellValue

  private var callbacks: List[() => Unit] = Nil
  def registerOnUpdate(callback:  => Unit): Unit = {
    callbacks ::= (() => callback)
  }

  protected def recomputeValue(changeHint: ChangeHint): A

  def update(changeHint: ChangeHint): Unit = {
    val newValue = recomputeValue(changeHint)
    if (newValue != currentCellValue) {
      _currentCellValue = newValue
      for (c <- callbacks) {
        c()
      }
    } else {
      // don't update, don't notify anyone
      // (empty branch for clearer coverage reports)
    }
  }

}