import Backgammon.GameException.{MoveDestinationNotAvailable, MoveTargetNotAvailable}
import Backgammon.{Board, CELLS, Chip, Move}
import Backgammon.Player.{BLACK, WHITE}
import org.scalatest.{FunSpec, Matchers}

class BoardSpec extends FunSpec with Matchers {

  val startBoard: Board = Board(Map(
    WHITE -> (1 to Backgammon.CHIPS).map(_ => Chip(0)).toList,
    BLACK -> (1 to Backgammon.CHIPS).map(_ => Chip(0)).toList,
  ))

  it("can move to non-occupied") {
    startBoard
      .move(Move(WHITE, 0, 6))
      .flatMap(_.move(Move(WHITE, 0, 6)))
      .flatMap(_.move(Move(WHITE, 6, 5)))
      .isRight shouldBe true
  }

  it("both players can step to the end") {
    startBoard
      .move(Move(WHITE, 0, CELLS - 1))
      .flatMap(_.move(Move(BLACK, 0, CELLS / 2 - 1)))
      .isRight shouldBe true
  }

  it("can't step on opponents chip 1") {
    startBoard.move(Move(WHITE, 0, 12)) shouldBe Left(MoveDestinationNotAvailable)
  }

  it("can't step on opponents chip 2") {
    startBoard
      .move(Move(WHITE, 0, 6))
      .flatMap(_.move(Move(BLACK, 0, 2)))
      .flatMap(_.move(Move(WHITE, 6, 14))) shouldBe Left(MoveDestinationNotAvailable)
  }

  it("can't move opponent's chip") {
    startBoard
      .move(Move(WHITE, 0, 6))
      .flatMap(_.move(Move(BLACK, 6, 2))) shouldBe Left(MoveTargetNotAvailable)
  }

  it("can't move from empty cell") {
    startBoard.move(Move(WHITE, 1, 6)) shouldBe Left(MoveTargetNotAvailable)
  }
}
