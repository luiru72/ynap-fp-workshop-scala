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

object BuiltinTryTests extends SimpleTestSuite {

  import scala.util.{Failure, Success, Try}

  case class Qty(value: Int)

  case class InvalidQtyException(value: String) extends RuntimeException(s"invalid quantity value: $value")

  def toQty(value: String): Try[Qty] =
    if (value.matches("^[0-9]+$")) Success(Qty(value.toInt))
    else Failure(InvalidQtyException(value))

  test("valid qty") {
    assertEquals(toQty("100"), Success(Qty(100)))
  }

  test("invalid qty") {
    assertEquals(toQty("asd"), Failure(InvalidQtyException("asd")))
    assertEquals(toQty("1 0 0"), Failure(InvalidQtyException("1 0 0")))
    assertEquals(toQty(""), Failure(InvalidQtyException("")))
    assertEquals(toQty("-10"), Failure(InvalidQtyException("-10")))
  }
}
