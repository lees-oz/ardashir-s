  object Backgammon {

  val CELLS = 24
  val CHIPS = 12

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

  case class Move(player: Player, from: Int, steps: Int)

  case class Chip(path: Int)

  case class Board(chips: Map[Player, List[Chip]]) {
    def move(m: Move): Either[Error, Board] = {
      for {
        targetChipIdx <- findChip(m)
        // find new path, if available - Option[Int]
        // if finishing path, all chips should be in dome
        _ <- findDest(m)
      } yield Board(Map(
        m.player -> chips(m.player).updated(targetChipIdx, Chip(m.from + m.steps)),
        m.player.opponent -> chips(m.player.opponent),
      ))
    }

    private def findChip(move: Move): Either[Error, Int] = {
      val targetChipIdx = chips(move.player).indexWhere(_.path == move.from)
      if (targetChipIdx != -1) Right(targetChipIdx)
      else Left(new Error(s"No chip available for player ${move.player} at path ${move.from}."))
    }

    private def findDest(move: Move): Either[Error, Unit] = {
      val dest = Math.min(move.from + move.steps, CELLS)
      if (dest == CELLS || !chips(move.player.opponent).exists(_.path == dest - CELLS / 2)) Right(())
      else Left(new Error(s"There's opponent's chip at destination path $dest"))
    }

    private def winner: Option[Player] = chips
      .find((p: (Player, List[Chip])) => p._2.forall(_.path == CELLS)).map(_._1)
  }
}
