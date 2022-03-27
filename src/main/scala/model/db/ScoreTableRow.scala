package model.db

import model.{Score, Player, State}
import cats.data.Validated._
import cats.data.{Validated, ValidatedNel}
import cats.data.NonEmptyList
import cats.implicits._
import shapeless._
import shapeless.ops.record._

case class ScoreTableRow(id: Long, data: ScoreTableRowWithoutId)

case class ScoreTableRowWithoutId(
    isDeuce: Boolean,
    playerWithAdvantage: Option[Int],
    playerThatWon: Option[Int],
    p1Score: Int,
    p2Score: Int
)

sealed trait StateParseError
case class MultiplePossibleStatesFound(states: List[model.State])
    extends StateParseError
case object NoStatesFound extends StateParseError
// case class InvalidScores(p1Score: Int, p2Score: Int) extends StateParseError
case class InvalidAdvantagePlayer(player: Int) extends StateParseError
case class InvalidWinPlayer(player: Int) extends StateParseError
case class InvalidPlayer1Score(p1Raw: Int) extends StateParseError
case class InvalidPlayer2Score(p2Raw: Int) extends StateParseError

object ScoreTableRowWithoutId {
  def columns: List[String] = ???

  def fromState(state: State): ScoreTableRowWithoutId = ???

  def toState(
      st: ScoreTableRowWithoutId
  ): ValidatedNel[StateParseError, model.State] = {
    val validatedNelStates: ValidatedNel[StateParseError, List[model.State]] = ???

    def ensureOnlyASingleStateExists(
        states: List[model.State]
    ): ValidatedNel[StateParseError, model.State] = ???

    validatedNelStates match {
      case Invalid(errs) => Invalid(errs)
      case Valid(states) => { ensureOnlyASingleStateExists(states) }
    }
  }
}
