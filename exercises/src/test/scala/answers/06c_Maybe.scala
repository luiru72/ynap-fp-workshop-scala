package exercises.answers

import minitest._

/*
 * Functions can't always return a value.
 * In this scenario they are called: partial functions.
 * We can convert them into total functions
 * with the introduction of effects.
 *
 *  f:  InType => Effect[OutType]
 */

object BuiltinMaybeTests extends SimpleTestSuite {

  case class Qty(value: Int)

  def toQty(value: String): Option[Qty] =
    if (value.matches("^[0-9]+$")) Some(Qty(value.toInt))
    else None

  test("valid qty") {
    assertEquals(toQty("100"), Some(Qty(100)))
  }

  test("invalid qty") {
    assertEquals(toQty("asd"), None)
    assertEquals(toQty("1 0 0"), None)
    assertEquals(toQty(""), None)
    assertEquals(toQty("-10"), None)
  }
}
