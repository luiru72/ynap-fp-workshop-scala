package exercises

import minitest._

object MarsRoverKataTests extends SimpleTestSuite {

  case class Position(x: Int, y: Int)
  case class Size(x: Int, y: Int)
  case class Rover(position: Position, direction: Direction)
  case class Planet(size: Size, rover: Rover)

  sealed trait Command
  case object F extends Command
  case object B extends Command
  case object L extends Command
  case object R extends Command

  sealed trait Direction
  case object N extends Direction
  case object E extends Direction
  case object W extends Direction
  case object S extends Direction

  def program(input: String, planet: Planet): Planet = ???

  def parse(chars: List[Char]): List[Command] = ???

  def execute(cmd: Command, planet: Planet): Planet = ???

  test("it works!") {
    val p        = Planet(Size(10, 10), Rover(Position(0, 0), N))
    val input    = "rrfff"
    val result   = program(input, p)
    val expected = Planet(Size(10, 10), Rover(Position(3, 0), S))
    assertEquals(result, expected)
  }
}
