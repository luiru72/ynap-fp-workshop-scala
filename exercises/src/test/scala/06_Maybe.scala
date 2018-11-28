package exercises

import minitest._

/*
 * Functions can't always return a value.
 * In this scenario they are called: partial functions.
 * We can convert them into total functions
 * with the introduction of effects.
 *
 *  f:  InType => Effect[OutType]
 */

object MaybeTests extends SimpleTestSuite {

  /*
   * TODO: remove all nulls
   */

  case class Qty(value: Int)

  sealed trait FQty[+A]
  case class ValidQty[A](qty: Qty) extends FQty[A]
  case object InvalidQty extends FQty[Nothing]

  def toQty(value: String): FQty[Qty] =
    if (value.matches("^[0-9]+$")) ValidQty(Qty(value.toInt))
    else InvalidQty

  test("valid qty") {
    assertEquals(toQty("100"), ValidQty(Qty(100)))
  }

  test("invalid qty") {
    assertEquals(toQty("asd"), InvalidQty)
    assertEquals(toQty("1 0 0"), InvalidQty)
    assertEquals(toQty(""), InvalidQty)
    assertEquals(toQty("-10"), InvalidQty)
  }

}
