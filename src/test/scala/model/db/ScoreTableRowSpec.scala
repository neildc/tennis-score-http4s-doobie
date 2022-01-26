package model.db

import org.specs2.mutable._
import model.{State, Player}
import model.db.ScoreTableRow
import cats.data.Validated._
import cats.data.NonEmptyList

class HelloWorldSpec extends Specification {

  "ScoreTableRow" should {
    "Deuce round trip conversions" in {
      val input = model.Deuce
      ScoreTableRow.toState(ScoreTableRow.fromState(1, input)) must equalTo(
        Valid(input)
      )
    }
    "Advantage round trip conversions P1" in {
      val input = model.Advantage(model.P1)
      ScoreTableRow.toState(ScoreTableRow.fromState(1, input)) must equalTo(
        Valid(input)
      )
    }
    "Advantage round trip conversions P2" in {
      val input = model.Advantage(model.P2)
      ScoreTableRow.toState(ScoreTableRow.fromState(2, input)) must equalTo(
        Valid(input)
      )
    }
    "Win round trip conversions P1" in {
      val input = model.Win(model.P1)
      ScoreTableRow.toState(ScoreTableRow.fromState(1, input)) must equalTo(
        Valid(input)
      )
    }
    "Win round trip conversions P2" in {
      val input = model.Win(model.P2)
      ScoreTableRow.toState(ScoreTableRow.fromState(2, input)) must equalTo(
        Valid(input)
      )
    }
    "Score round trip conversions" in {
      val input = model.NormalScoring(model.Score15, model.Score30)
      ScoreTableRow.toState(ScoreTableRow.fromState(1, input)) must equalTo(
        Valid(input)
      )
    }
    "Reports when multiple valid states are found (3)" in {
      val input = ScoreTableRow(
        id = 1,
        isDeuce = true,
        playerWithAdvantage = Some(2),
        playerThatWon = None,
        p1Score = 2,
        p2Score = 2
      )
      ScoreTableRow.toState(input) must equalTo(
        invalidNel(
          ScoreTableRow.MultiplePossibleStatesFound(
            List(
              model.Deuce,
              model.Advantage(model.P2),
              model.NormalScoring((model.Score30, model.Score30))
            )
          )
        )
      )
    }

    "Reports when multiple valid states are found (4)" in {
      val input = ScoreTableRow(
        id = 1,
        isDeuce = true,
        playerWithAdvantage = Some(1),
        playerThatWon = Some(2),
        p1Score = 2,
        p2Score = 2
      )
      ScoreTableRow.toState(input) must equalTo(
        Invalid(
          NonEmptyList.of(
            ScoreTableRow.MultiplePossibleStatesFound(
              List(
                model.Deuce,
                model.Advantage(model.P1),
                model.Win(model.P2),
                model.NormalScoring((model.Score30, model.Score30))
              )
            )
          )
        )
      )
    }
    "Reports when multiple errors are found" in {
      val input = ScoreTableRow(
        id = 1,
        isDeuce = true,
        playerWithAdvantage = Some(9),
        playerThatWon = Some(4),
        p1Score = 11,
        p2Score = 22
      )

      ScoreTableRow.toState(input) must equalTo(
        Invalid(
          NonEmptyList.of(
            ScoreTableRow.InvalidAdvantagePlayer(9),
            ScoreTableRow.InvalidWinPlayer(4),
            ScoreTableRow.InvalidPlayer1Score(11),
            ScoreTableRow.InvalidPlayer2Score(22)
          )
        )
      )
    }
  }
}
