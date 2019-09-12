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
      .move(Move(WHITE, 0, CELLS))
      .flatMap(_.move(Move(BLACK, 0, CELLS / 2)))
      .isRight shouldBe true
  }

  it("can't step on opponents chip 1") {
    startBoard
      .move(Move(WHITE, 0, 12))
      .isLeft shouldBe true
  }

  it("can't step on opponents chip 2") {
    startBoard
      .move(Move(WHITE, 0, 6))
      .flatMap(_.move(Move(BLACK, 0, 2)))
      .flatMap(_.move(Move(WHITE, 6, 8)))
      .isLeft shouldBe true
  }


}
