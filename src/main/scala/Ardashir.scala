import Ardashir.Player.{BLACK, WHITE}
import Ardashir.Stack.PlayerStack

object Ardashir extends App {

  val CELLS = 24

  sealed trait Player

  object Player {
    final case object WHITE extends Player
    final case object BLACK extends Player
  }


  sealed trait Stack
  object Stack {
    case object Empty extends Stack
    case class PlayerStack(player: Player, chips: Int) extends Stack
  }

  case class Dice(value: Int)

  case class Board(cells: List[Stack])



  case class Move(player: Player, from: Int, to: Int)




  val startBoard = Board(List(
    PlayerStack(WHITE, 12),
    Stack.Empty,
    Stack.Empty,
    Stack.Empty,
    Stack.Empty,
    Stack.Empty,
    Stack.Empty,
    Stack.Empty,
    Stack.Empty,
    Stack.Empty,
    Stack.Empty,
    Stack.Empty,
    PlayerStack(BLACK, 12),
    Stack.Empty,
    Stack.Empty,
    Stack.Empty,
    Stack.Empty,
    Stack.Empty,
    Stack.Empty,
    Stack.Empty,
    Stack.Empty,
    Stack.Empty,
    Stack.Empty,
    Stack.Empty,
  ))


  println(startBoard.cells.size)
}
