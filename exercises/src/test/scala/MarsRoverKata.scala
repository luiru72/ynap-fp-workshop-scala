

// http://kata-log.rocks/mars-rover-kata

package exercises

import minitest._

sealed trait Direction

object North extends Direction

object South extends Direction

object East extends Direction

object West extends Direction

sealed trait Command

sealed trait MoveCommand extends Command

object Forward extends MoveCommand

object Backward extends MoveCommand

sealed trait TurnCommand extends Command

object Left extends TurnCommand

object Right extends TurnCommand

object Invalid extends Command

case class Position(x: Int, y: Int)

// positions from 0 to maxX-1 and 0 to maxY-1
// top-left is (0,0), bottom-right is (maxX-1, maxY-1)
case class Planet(maxX: Int, maxY: Int)

case class Rover(direction: Direction, position: Position, planet: Planet)

object MarsRoverKata extends SimpleTestSuite {

  def move(rover: Rover, moveCommand: MoveCommand): Rover = {
    val (sx: Int, sy: Int) = rover.direction match {
      case North => moveCommand match {
        case Forward => (0, -1)
        case Backward => (0, 1)
      }
      case South => moveCommand match {
        case Forward => (0, 1)
        case Backward => (0, -1)
      }
      case East => moveCommand match {
        case Forward => (-1, 0)
        case Backward => (1, 0)
      }
      case West => moveCommand match {
        case Forward => (1, 0)
        case Backward => (-1, 0)
      }
    }
    calculatePosition(rover, sx, sy)
  }

  def calculatePosition(rover: Rover, sx: Int, sy: Int): Rover = {
    val newX = (rover.position.x + sx) % rover.planet.maxX
    val newY = (rover.position.y + sy) % rover.planet.maxY
    rover.copy(position = Position(newX, newY))
  }

  def calculatePositionCommon(x: Int, sx: Int, maxX: Int): Int = {
    val newX = x + sx
    val modX = newX % maxX
    if (modX >= 0)
      modX
    else
      modX + maxX + (maxX * ((-modX.toFloat / maxX.toFloat).floor.toInt))
  }

  def turn(rover: Rover, turnCommand: TurnCommand): Rover = {
    val newDirection = rover.direction match {
      case North => turnCommand match {
        case Left => West
        case Right => East
      }
      case East => turnCommand match {
        case Left => North
        case Right => South
      }
      case South => turnCommand match {
        case Left => East
        case Right => West
      }
      case West => turnCommand match {
        case Left => South
        case Right => North
      }
    }
    rover.copy(direction = newDirection)
  }

  def fromCharToCommand(c: Char): Command = c match {
    case 'L' => Left
    case 'R' => Right
    case 'F' => Forward
    case 'B' => Backward
    case _ => Invalid
  }

  def parseString(commands: String): List[Command] =
    commands.foldLeft(List(): List[Command])((cmdList: List[Command], char: Char) => cmdList :+ fromCharToCommand(char))

  def executeCommand(rover: Rover, command: Command): Rover = command match {
    case Left => turn(rover, Left)
    case Right => turn(rover, Right)
    case Forward => move(rover, Forward)
    case Backward => move(rover, Backward)
    case Invalid => rover
  }

  def executeCommands(rover: Rover, commands: String): Rover = {
    commands.foldLeft(rover)((rover: Rover, command: Char) => executeCommand(rover, fromCharToCommand(command)))
  }




  test("it works!") {
    val initialRover = Rover(South, Position(0, 0), Planet(10, 10))
    val expected = Rover(South, Position(0, 1), Planet(10, 10))
    val result = move(initialRover, Forward)
    assertEquals(result, expected)
  }

  test("Test Parsing") {
    val commands = "FBLRI"
    val result = parseString(commands)
    val expected = List(Forward, Backward, Left, Right, Invalid)
    assertEquals(result, expected)
  }

  test("Test execute commands") {
    val commands = "FBLRI"
    val result = parseString(commands)
    val expected = List(Forward, Backward, Left, Right, Invalid)
    assertEquals(result, expected)
  }

  test("calculatePositionCommon") {
    assertEquals(calculatePositionCommon(0, 1, 4), 1)
    assertEquals(calculatePositionCommon(2, -1, 3),1)
    assertEquals(calculatePositionCommon(2, 1, 3), 0)
    assertEquals(calculatePositionCommon(0, -1, 3), 2)
    assertEquals(calculatePositionCommon(0, -10, 3), 2)
  }

}
