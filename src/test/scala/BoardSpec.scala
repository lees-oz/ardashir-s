import Backgammon.GameException.{AllChipsMustBeInDome, MoveDestinationNotAvailable, MoveTargetNotAvailable, ShouldMoveForward}
import Backgammon.Player.{BLACK, WHITE}
import Backgammon.{BackgammonConfig, Board, Move}
import org.scalatest.{FunSpec, Matchers}

class BoardSpec extends FunSpec with Matchers {

  describe("classic board") {
    val classicBoard: Board = Board.init(BackgammonConfig(24, 12))

    it("can move to non-occupied and forward") {
      classicBoard
        .move(Move(WHITE, 0, 6))
        .flatMap(_.move(Move(WHITE, 6, 8)))
        .flatMap(_.move(Move(BLACK, 0, 6)))
        .flatMap(_.move(Move(BLACK, 6, 8))).isRight shouldBe true
    }

    it("can move to non-occupied and non-forward") {
      classicBoard
        .move(Move(WHITE, 0, 6))
        .flatMap(_.move(Move(WHITE, 0, 6)))
        .flatMap(_.move(Move(WHITE, 6, 5))) shouldBe Left(ShouldMoveForward)
    }

    it("can step to end only if all chips in dome") {
      classicBoard
        .move(Move(WHITE, 0, 23))
        .flatMap(_.move(Move(WHITE, 0, 23)))
        .flatMap(_.move(Move(WHITE, 0, 23)))
        .flatMap(_.move(Move(WHITE, 0, 23)))
        .flatMap(_.move(Move(WHITE, 0, 23)))
        .flatMap(_.move(Move(WHITE, 0, 23)))
        .flatMap(_.move(Move(WHITE, 0, 23)))
        .flatMap(_.move(Move(WHITE, 0, 23)))
        .flatMap(_.move(Move(WHITE, 0, 23)))
        .flatMap(_.move(Move(WHITE, 0, 23)))
        .flatMap(_.move(Move(WHITE, 0, 23)))
        .flatMap(_.move(Move(WHITE, 0, 23)))
        .flatMap(_.move(Move(WHITE, 23, 24)))
        .isRight shouldBe true
    }

    it("can't step to end if not all chips in dome") {
      classicBoard
        .move(Move(WHITE, 0, 24)) shouldBe Left(AllChipsMustBeInDome)
    }

    it("can't step to end when at least one isn't in dome") {
      classicBoard
        .move(Move(WHITE, 0, 23))
        .flatMap(_.move(Move(WHITE, 0, 23)))
        .flatMap(_.move(Move(WHITE, 0, 23)))
        .flatMap(_.move(Move(WHITE, 0, 23)))
        .flatMap(_.move(Move(WHITE, 0, 23)))
        .flatMap(_.move(Move(WHITE, 0, 23)))
        .flatMap(_.move(Move(WHITE, 0, 23)))
        .flatMap(_.move(Move(WHITE, 0, 23)))
        .flatMap(_.move(Move(WHITE, 0, 23)))
        .flatMap(_.move(Move(WHITE, 0, 23)))
        .flatMap(_.move(Move(WHITE, 0, 23)))
        .flatMap(_.move(Move(WHITE, 23, 24))) shouldBe Left(AllChipsMustBeInDome)
    }


    it("can't step on opponents chip 1") {
      classicBoard.move(Move(WHITE, 0, 12)) shouldBe Left(MoveDestinationNotAvailable)
    }

    it("can't step on opponents chip 2") {
      classicBoard
        .move(Move(WHITE, 0, 6))
        .flatMap(_.move(Move(BLACK, 0, 2)))
        .flatMap(_.move(Move(WHITE, 6, 14))) shouldBe Left(MoveDestinationNotAvailable)
    }

    it("can't move opponent's chip") {
      classicBoard
        .move(Move(WHITE, 0, 6))
        .flatMap(_.move(Move(BLACK, 6, 8))) shouldBe Left(MoveTargetNotAvailable)
    }

    it("can't move from empty cell") {
      classicBoard.move(Move(WHITE, 1, 6)) shouldBe Left(MoveTargetNotAvailable)
    }
  }
}
