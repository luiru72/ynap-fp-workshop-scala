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

object CustomGenericMaybeTests extends SimpleTestSuite {

  sealed trait Maybe[+A]
  case class Yeah[A](value: A) extends Maybe[A]
  case class Nope()            extends Maybe[Nothing]

  case class Qty(value: Int)

  def toQty(value: String): Maybe[Qty] =
    if (value.matches("^[0-9]+$")) Yeah(Qty(value.toInt))
    else Nope()

  test("valid qty") {
    assertEquals(toQty("100"), Yeah(Qty(100)))
  }

  test("invalid qty") {
    assertEquals(toQty("asd"), Nope())
    assertEquals(toQty("1 0 0"), Nope())
    assertEquals(toQty(""), Nope())
    assertEquals(toQty("-10"), Nope())
  }
}
