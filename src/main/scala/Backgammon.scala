import Backgammon.GameException.{MoveDestinationNotAvailable, MoveTargetNotAvailable}

object Backgammon {

  val CELLS = 24
  val CHIPS = 12

  sealed trait GameException extends Throwable
  object GameException {
    case object MoveTargetNotAvailable extends GameException
    case object MoveDestinationNotAvailable extends GameException
    case object AllChipsMustBeInDome extends GameException
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

  case class Board(chips: Map[Player, List[Chip]]) {
    def move(m: Move): Either[GameException, Board] = {
      for {
        chipIndex <- findChip(m)
        // find new path, if available - Option[Int]
        // if finishing path, all other chips should be in dome
        _ <- findDest(m)
      } yield Board(Map(
        m.player -> chips(m.player).updated(chipIndex, Chip(m.to)),
        m.player.opponent -> chips(m.player.opponent),
      ))
    }

    private def findChip(move: Move): Either[GameException, Int] = {
      val targetChipIdx = chips(move.player).indexWhere(_.path == move.from)
      if (targetChipIdx != -1) Right(targetChipIdx)
      else Left(MoveTargetNotAvailable)
    }

    private def findDest(move: Move): Either[GameException, Unit] = {
      val dest = Math.min(move.to, CELLS - 1)
      if (dest == CELLS - 1 || !chips(move.player.opponent).exists(_.path == dest - CELLS / 2)) Right(())
      else Left(MoveDestinationNotAvailable)
    }

    private def winner: Option[Player] = chips
      .find((p: (Player, List[Chip])) => p._2.forall(_.path == CELLS)).map(_._1)
  }
}
