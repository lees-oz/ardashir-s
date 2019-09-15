import Backgammon.GameException.{AllChipsMustBeInDome, MoveDestinationNotAvailable, MoveTargetNotAvailable}
import Backgammon.Player.{BLACK, WHITE}

object Backgammon {
  sealed trait GameException extends Throwable
  object GameException {
    final case object MoveTargetNotAvailable extends GameException
    final case object MoveDestinationNotAvailable extends GameException
    final case object AllChipsMustBeInDome extends GameException
  }

  sealed trait Player {
    def opponent: Player
  }

  object Player {
    final case object WHITE extends Player {
      override def opponent: Player = BLACK
    }
    final case object BLACK extends Player {
      override def opponent: Player = WHITE
    }
  }

  case class Move(player: Player, from: Int, to: Int)

  case class Chip(path: Int)

  case class BackgammonConfig(cells: Int, chips: Int)

  case class Board(chips: Map[Player, List[Chip]])(implicit val config: BackgammonConfig) {
    private val CELLS = config.cells
    private val CHIPS = config.chips

    def move(m: Move): Either[GameException, Board] = {
      for {
        chipIndex <- findTargetChip(m)
        _ <- findDestination(m)
        _ <- allChipsMustBeInDome(m)
      } yield Board(Map(
        m.player -> chips(m.player).updated(chipIndex, Chip(m.to)),
        m.player.opponent -> chips(m.player.opponent),
      ))
    }

    private def findTargetChip(move: Move): Either[GameException, Int] = {
      val targetChipIdx = chips(move.player).indexWhere(_.path == move.from)
      if (targetChipIdx != -1) Right(targetChipIdx)
      else Left(MoveTargetNotAvailable)
    }

    private def findDestination(move: Move): Either[GameException, Unit] = {
      val dest = Math.min(move.to, CELLS)
      if (dest < CELLS && chips(move.player.opponent).exists(_.path == dest - CELLS / 2)) Left(MoveDestinationNotAvailable)
      else Right(())
    }

    private def allChipsMustBeInDome(move: Move): Either[GameException, Unit] = {
      if (move.to >= CELLS && chips(move.player).exists(_.path < CELLS / 2)) Left(AllChipsMustBeInDome)
      else Right(())
    }

    private def winner: Option[Player] = chips
      .find((p: (Player, List[Chip])) => p._2.forall(_.path == CELLS)).map(_._1)
  }

  object Board {
    def init(implicit config: BackgammonConfig): Board = {
      Board(Map(
        WHITE -> (1 to config.chips).map(_ => Chip(0)).toList,
        BLACK -> (1 to config.chips).map(_ => Chip(0)).toList,
      ))
    }
  }
}
