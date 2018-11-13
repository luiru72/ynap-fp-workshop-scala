package exercises

import minitest._

/*
 * In OOP model object that incapsulate data and expose behaviours.
 * This two concepts are brigs togheter thanks to class definitions.
 *
 * In FP data and behaviours are modelled with two different tools:
 * - Algebraic Data Type (ADT) to model data
 * - Function to model behaviours
 *
 * An ADT is an immutable data structure that compose other types.
 * There are two common kinds of composition strategy:
 * - Product type: put many types togheter. e.g. struct in C, POJO in JAVA.
 *                 It's called product because all the possible values of a Tuple[String, Int] is
 *                 the *product* of all possible String with all possible Int.
 *                 Useful to model indipendent data in AND e.g. a Person is composed by a name *and* an age.
 *
 * - Sum type:     model exclusive types e.g. union in C, enum in JAVA.
 *                 Sum types correspond to disjoint unions of sets.
 *                 It's called sum because all the possible values of a Either[String, Int] is
 *                 the *sum* of all possible String with all possible Int.
 *                 Useful to model dipendant data in OR e.g. the Light is on *or* off.
 *
 * We can mix ADT as we want, like a product type that compose a type with a sum type.
 */

object ModelData extends SimpleTestSuite {

  // Typical product type in Scala
  case class Person(name: String, age: Int)

  // Typical sum type in Scala
  sealed trait LightState
  case class On()  extends LightState
  case class Off() extends LightState

  /*
   * TODO: Model Scopa the italian card game, below the game description. :-)
   *       After modeling the domain implements the test following the description of ignores.
   *
   * DESCIPTION:
   *       It is played (let simplify) between two players with
   *       a standard Italian 40-card deck, divided into four suits: Cups, Golds, Swords, Clubs.
   *       The values on the cards range numerically from one through seven,
   *       plus three face cards in each suit: Knight (worth 8), Queen (worth 9) and King (worth 10).
   *       Each player receives three cards. The dealer will also place four cards face up on the table.
   *
   * ADD YOUR CODE HERE INSIDE THE OBJECT
   */

  test("represent initial match state") {
    ignore("build the first player w/ 2 of Golds, 5 of Swords and 7 of Clubs")
    ignore("build the second player w/ 1 of Cups, 2 of Clubs and 9 of Golds")
    ignore("build the table w/ 4 of Clubs, 10 of Swords, 8 of Golds and 1 of Swords")
    ignore("build the deck w/ only 1 of Swords and 10 of Clubs")
    ignore("build the game")

    sealed trait Suit
    case object Cups extends Suit
    case object Golds extends Suit
    case object Swords extends Suit
    case object Clubs extends Suit

    sealed trait Value
    case object One extends Value
    case object Two extends Value
    case object Three extends Value
    case object Four extends Value
    case object Five extends Value
    case object Six extends Value
    case object Seven extends Value
    case object Knight extends Value
    case object Queen extends Value
    case object King extends Value

    case class Card(value: Value, suit: Suit)
    case class Deck(cards: List[Card])
    case class Table(cards: List[Card])
    case class Player(cards: List[Card])
    case class Game(deck: Deck, table: Table, player1: Player, player2: Player)

    def value(v: Value) = v match {
      case One => 1
      case Two => 2
      case Three => 3
      case Four => 4
      case Five => 5
      case Six => 6
      case Seven => 7
      case Knight => 8
      case Queen => 9
      case King => 10
    }

    val table: Table = new Table(List())
    val player1: Player = Player(List(Card(Two, Golds), Card(Five, Swords), Card(Seven, Clubs)))
    val player2: Player = Player(List(Card(One, Cups), Card(Two, Clubs), Card(Queen, Golds)))
    val deck: Deck = Deck(List(Card(One, Swords), Card(King, Clubs)))
    val game: Game = Game(deck, table, player1, player2)
    print(game.deck)
    print(value(One))
    () // don't delete
  }

}
