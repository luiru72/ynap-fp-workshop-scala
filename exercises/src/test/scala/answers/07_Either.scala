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

object BuiltinEitherTests extends SimpleTestSuite {

  type Error = String

  case class Qty(value: Int)

  def toQty(value: String): Either[Error, Qty] =
    if (value.matches("^[0-9]+$")) Right(Qty(value.toInt))
    else Left(s"invalid quantity value: $value")

  test("valid qty") {
    assertEquals(toQty("100"), Right(Qty(100)))
  }

  test("invalid qty") {
    assertEquals(toQty("asd"), Left("invalid quantity value: asd"))
    assertEquals(toQty("1 0 0"), Left("invalid quantity value: 1 0 0"))
    assertEquals(toQty(""), Left("invalid quantity value: "))
    assertEquals(toQty("-10"), Left("invalid quantity value: -10"))
  }
}
