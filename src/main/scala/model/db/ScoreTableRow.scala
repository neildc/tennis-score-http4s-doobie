package model.db

import model.{State, Player, Score, NormalScoring}
import cats.data.Validated._
import cats.data.{Validated, ValidatedNel}
import cats.data.NonEmptyList
import cats.implicits._

case class ScoreTableRow(
        id: Long,
        isDeuce: Boolean,
        playerWithAdvantage: Option[Int],
        playerThatWon: Option[Int],
        p1Score: Int,
        p2Score: Int
    )

object ScoreTableRow {

  def fromState(id: Long, state: State): ScoreTableRow = {
    val baseRow =
      ScoreTableRow(
        id = id,
        isDeuce = false,
        playerWithAdvantage = None,
        playerThatWon = None,
        p1Score = 0,
        p2Score = 0
      )

    state match {
      case model.NormalScoring((p1Score, p2Score)) =>
        baseRow.copy(
          p1Score = model.scoreToInt(p1Score),
          p2Score = model.scoreToInt(p2Score)
        )
      case model.Deuce =>
        baseRow.copy(
          isDeuce = true,
          p1Score = 3,
          p2Score = 3
        )
      case model.Advantage(player) =>
        baseRow.copy(
          playerWithAdvantage = Some(model.playerToInt(player)),
          p1Score = 3,
          p2Score = 3
        )
      case model.Win(player) =>
        baseRow.copy(
          playerThatWon = Some(model.playerToInt(player))
        )
    }

  }

  sealed trait StateParseError
  case class MultiplePossibleStatesFound(states: List[State])
      extends StateParseError
  case object NoStatesFound extends StateParseError
  // case class InvalidScores(p1Score: Int, p2Score: Int) extends StateParseError
  case class InvalidAdvantagePlayer(player: Int) extends StateParseError
  case class InvalidWinPlayer(player: Int) extends StateParseError
  case class InvalidPlayer1Score(p1Raw: Int) extends StateParseError
  case class InvalidPlayer2Score(p2Raw: Int) extends StateParseError

  def toState(
      st: ScoreTableRow
  ): ValidatedNel[StateParseError, State] = {
    val optDeuce = if (st.isDeuce) Some(model.Deuce) else None

    val optAdvantage: Option[ValidatedNel[StateParseError, model.Advantage]] =
      st.playerWithAdvantage.map(playerInt =>
        model.intToPlayer(playerInt) match {
          case None         => invalidNel(InvalidAdvantagePlayer(playerInt))
          case Some(player) => Valid(model.Advantage(player))
        }
      )

    val optWin: Option[ValidatedNel[StateParseError, model.Win]] =
      st.playerThatWon.map(playerInt => {

        model.intToPlayer(playerInt) match {
          case None         => invalidNel(InvalidWinPlayer(playerInt))
          case Some(player) => Valid(model.Win(player))
        }
      })

    val optNormalScore: ValidatedNel[StateParseError, model.NormalScoring] =
      (model.intToScore(st.p1Score), model.intToScore(st.p2Score)) match {
        case (Some(p1Score), Some(p2Score)) =>
          Valid(model.NormalScoring((p1Score, p2Score)))

        case (None, None) =>
          Invalid(
            NonEmptyList.of(
              InvalidPlayer1Score(st.p1Score),
              InvalidPlayer2Score(st.p2Score)
            )
          )
        case (None, _) => invalidNel(InvalidPlayer1Score(st.p1Score))
        case (_, None) => invalidNel(InvalidPlayer2Score(st.p2Score))
      }

    (optDeuce, optAdvantage, optWin, optNormalScore) match {
      case (Some(deuce), None, None, Valid(_))     => Valid(deuce)
      case (None, Some(Valid(advantage)), None, _) => Valid(advantage)
      case (None, None, Some(Valid(win)), _)       => Valid(win)
      case (_) => {
        val b: ValidatedNel[StateParseError, List[State]] =
          List(
            optDeuce.map(_.valid),
            optAdvantage,
            optWin,
            Some(optNormalScore)
          ).flatten.sequence

        b match {
          case Invalid(errs) => Invalid(errs)
          case Valid(states) => {
            optNormalScore match {
              case Invalid(_) =>
                if (states.length > 1) {
                  invalidNel(MultiplePossibleStatesFound(states))
                } else {
                  invalidNel(NoStatesFound)
                }
              case Valid(score) => {
                if (states.length > 2) {
                  invalidNel(MultiplePossibleStatesFound(states))
                } else {
                  Valid(score)
                }
              }
            }
          }
        }
      }
    }
  }
}
