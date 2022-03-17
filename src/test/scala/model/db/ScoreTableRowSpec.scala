package model.db

import org.specs2.mutable._
import model.{State, Player}
import model.db.{ScoreTableRow, ScoreTableRowWithoutId, StateParseError}
import cats.data.Validated._
import cats.data.NonEmptyList

class HelloWorldSpec extends Specification {

  "ScoreTableRow" should {
    "Deuce round trip conversions" in {
      val input = model.Deuce
      ScoreTableRowWithoutId.toState(
        ScoreTableRowWithoutId.fromState(input)
      ) must equalTo(
        Valid(input)
      )
    }
    "Advantage round trip conversions P1" in {
      val input = model.Advantage(model.P1)
      ScoreTableRowWithoutId.toState(
        ScoreTableRowWithoutId.fromState(input)
      ) must equalTo(
        Valid(input)
      )
    }
    "Advantage round trip conversions P2" in {
      val input = model.Advantage(model.P2)
      ScoreTableRowWithoutId.toState(
        ScoreTableRowWithoutId.fromState(input)
      ) must equalTo(
        Valid(input)
      )
    }
    "Win round trip conversions P1" in {
      val input = model.Win(model.P1)
      ScoreTableRowWithoutId.toState(
        ScoreTableRowWithoutId.fromState(input)
      ) must equalTo(
        Valid(input)
      )
    }
    "Win round trip conversions P2" in {
      val input = model.Win(model.P2)
      ScoreTableRowWithoutId.toState(
        ScoreTableRowWithoutId.fromState(input)
      ) must equalTo(
        Valid(input)
      )
    }
    "Score round trip conversions" in {
      val input = model.NormalScoring((model.ScoreLove, model.Score15))
      ScoreTableRowWithoutId.toState(
        ScoreTableRowWithoutId.fromState(input)
      ) must equalTo(
        Valid(input)
      )

      val input2 = model.NormalScoring((model.Score30, model.Score40))
      ScoreTableRowWithoutId.toState(
        ScoreTableRowWithoutId.fromState(input2)
      ) must equalTo(
        Valid(input2)
      )
    }

    "Reports when multiple valid states are found (3)" in {
      val input = ScoreTableRowWithoutId(
        isDeuce = true,
        playerWithAdvantage = Some(2),
        playerThatWon = None,
        p1Score = 2,
        p2Score = 2
      )

      ScoreTableRowWithoutId.toState(input) must equalTo(
        invalidNel(
          MultiplePossibleStatesFound(
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
      val input = ScoreTableRowWithoutId(
        isDeuce = true,
        playerWithAdvantage = Some(1),
        playerThatWon = Some(2),
        p1Score = 2,
        p2Score = 2
      )
      ScoreTableRowWithoutId.toState(input) must equalTo(
        Invalid(
          NonEmptyList.of(
            MultiplePossibleStatesFound(
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
      val input = ScoreTableRowWithoutId(
        isDeuce = true,
        playerWithAdvantage = Some(9),
        playerThatWon = Some(4),
        p1Score = 11,
        p2Score = 22
      )

      ScoreTableRowWithoutId.toState(input) must equalTo(
        Invalid(
          NonEmptyList.of(
            InvalidAdvantagePlayer(9),
            InvalidWinPlayer(4),
            InvalidPlayer1Score(11),
            InvalidPlayer2Score(22)
          )
        )
      )
    }
  }
}
