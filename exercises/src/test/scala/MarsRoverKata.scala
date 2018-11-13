// http://kata-log.rocks/mars-rover-kata

package exercises

import minitest._

sealed trait Direction
object N extends Direction
object S extends Direction
object E extends Direction
object W extends Direction

sealed trait Action

sealed trait MoveAction extends Action
object F extends MoveAction
object B extends MoveAction

sealed trait TurnAction extends Action
object L extends TurnAction
object R extends TurnAction

case class Position(x: Int, y: Int)

// positions from 0 to maxX-1 and 0 to maxY-1
// top-left is (0,0), bottom-right is (maxX-1, maxY-1)
case class Grid(maxX: Int, maxY: Int)

case class Rover(direction: Direction, position: Position, grid: Grid)

object MarsRoverKata extends SimpleTestSuite {

  def move(rover: Rover, moveAction: MoveAction): Rover = {
    val (sx: Int, sy: Int)  = rover.direction match {
      case N => moveAction match {
        case F => (0, -1)
        case B => (0, 1)
      }
      case S => moveAction match {
        case F => (0, 1)
        case B => (0, -1)
      }
      case E => moveAction match {
        case F => (-1, 0)
        case B => (1, 0)
      }
      case W => moveAction match {
        case F => (1, 0)
        case B => (-1, 0)
      }
    }
    rover.copy(position = Position(rover.position.x + sx, rover.position.y + sy))
  }

  def turn(rover: Rover, turnAction: MoveAction): Rover = ???
  def act(rover: Rover, action: Action): Rover = ???
  def addPosition(p: Position, x: Int, y: Int): Position = Position(p.x + x, p.y + y)

  test("it works!") {
    val initialRover = Rover(S, Position(0,0), Grid(10, 10))
    val result = move(initialRover, F)
    val expected = Rover(S, Position(0,1), Grid(10, 10))
    assertEquals(result, expected)
  }

}
