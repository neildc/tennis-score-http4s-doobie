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
  def columns: List[String] = {
    // https://svejcar.dev/posts/2019/10/22/extracting-case-class-field-names-with-shapeless/
    val gen = LabelledGeneric[ScoreTableRowWithoutId]
    Keys[gen.Repr].apply().toList.map(_.name)
  }

  def fromState(state: State): ScoreTableRowWithoutId = {
    val baseRow =
      ScoreTableRowWithoutId(
        isDeuce = false,
        playerWithAdvantage = None,
        playerThatWon = None,
        p1Score = 0,
        p2Score = 0
      )

    state match {
      case model.NormalScoring((p1Score, p2Score)) =>
        baseRow.copy(
          p1Score = Score.scoreToInt(p1Score),
          p2Score = Score.scoreToInt(p2Score)
        )
      case model.Deuce =>
        baseRow.copy(
          isDeuce = true,
          p1Score = 3,
          p2Score = 3
        )
      case model.Advantage(player) =>
        baseRow.copy(
          playerWithAdvantage = Some(Player.playerToInt(player)),
          p1Score = 3,
          p2Score = 3
        )
      case model.Win(player) =>
        baseRow.copy(
          playerThatWon = Some(Player.playerToInt(player))
        )
    }
  }

  def toState(
      st: ScoreTableRowWithoutId
  ): ValidatedNel[StateParseError, model.State] = {
    val optDeuce = if (st.isDeuce) Some(model.Deuce) else None

    val optAdvantage: Option[ValidatedNel[StateParseError, model.State]] =
      st.playerWithAdvantage.map(playerInt =>
        Player.intToPlayer(playerInt) match {
          case None         => invalidNel(InvalidAdvantagePlayer(playerInt))
          case Some(player) => Valid(model.Advantage(player))
        }
      )

    val optWin: Option[ValidatedNel[StateParseError, model.State]] =
      st.playerThatWon.map(playerInt => {

        Player.intToPlayer(playerInt) match {
          case Some(player) => Valid(model.Win(player))
          case None         => invalidNel(InvalidWinPlayer(playerInt))
        }
      })

    val normalScore: ValidatedNel[StateParseError, model.State] =
      (Score.intToScore(st.p1Score), Score.intToScore(st.p2Score)) match {
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

    val possiblyOptionalStates
        : List[Option[ValidatedNel[StateParseError, model.State]]] =
      List(optDeuce.map(_.valid), optAdvantage, optWin)

    val states: List[ValidatedNel[StateParseError, model.State]] =
      normalScore :: possiblyOptionalStates.flatten

    val validatedNelStates: ValidatedNel[StateParseError, List[model.State]] =
      states.sequence

    def ensureOnlyASingleStateExists(
        states: List[model.State]
    ): ValidatedNel[StateParseError, model.State] = {
      states.toNel match {
        case None                       => invalidNel(NoStatesFound)
        case Some(NonEmptyList(a, Nil)) => valid(a)
        case Some(_) => {
          if (states.length == 2)
            // We may have a normal score state alongside one of these 3
            // As long we only have one of any of those 3 than this is fine.
            (optDeuce, optAdvantage, optWin) match {
              case (Some(deuce), None, None)            => Valid(deuce)
              case (None, Some(Valid(advantage)), None) => Valid(advantage)
              case (None, None, Some(Valid(win)))       => Valid(win)
              case _ => invalidNel(MultiplePossibleStatesFound(states))
            }
          else
            invalidNel(MultiplePossibleStatesFound(states))
        }
      }
    }

    validatedNelStates match {
      case Invalid(errs) => Invalid(errs)
      case Valid(states) => { ensureOnlyASingleStateExists(states) }
    }
  }
}
