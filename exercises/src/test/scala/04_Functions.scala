package exercises

import minitest._

/*
 * ADT models data while Function models behaviour.
 * A function is simply something that accepts an input value
 * and produces an output value.
 * In more accademic terms it connects a Domain to a Codomain.
 * Functions are described/documented by it's type definition.
 *
 *  f:  InType => OutType
 */

object FunctionsTests extends SimpleTestSuite {

  /*
   * TODO: implements functions maked with `???`
   */

  val asString: Double => String = in => in.toString

  val parseString: String => Int = in => in.toInt

  val reciprocal: Int => Double = in => 1.0 / in

  val reciprocalString: String => String =
    asString.compose(reciprocal).compose(parseString)

  val reciprocalString2: String => String =
    parseString.andThen(reciprocal).andThen(asString)

  val reciprocalString3: String => String =
    parseString.andThen(reciprocal.andThen(asString))

  def andThen[A, B, C](f:A=>B, g: B=>C): A=>C =
    in => g(f(in))

  test("from string to string throught reciprocal") {
    assertEquals(reciprocalString("42"), "0.023809523809523808")
    assertEquals(reciprocalString2("42"), "0.023809523809523808")
    assertEquals(reciprocalString3("42"), "0.023809523809523808")
  }

  test("andThen") {
    assertEquals(andThen(reciprocal, asString)(10), (reciprocal andThen asString) (10))
  }

}
