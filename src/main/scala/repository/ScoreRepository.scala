package repository

import cats.effect.IO
import doobie.util.transactor.Transactor
import fs2.Stream
import model.{State, Player, Score, NormalScoring}
import doobie._
import doobie.implicits._
import cats.data.Validated._
import cats.data.{Validated, ValidatedNel}
import cats.data.NonEmptyList
import cats.implicits._
import model.Deuce

object ScoreRepository {
  case class ScoreTableRow(
      id: Long,
      isDeuce: Boolean,
      playerWithAdvantage: Option[Int],
      playerThatWon: Option[Int],
      p1Score: Int,
      p2Score: Int
  )

  def toScoreTableRow(id: Long, state: State): ScoreTableRow = {
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

  sealed trait ScoreRepositoryError
  case class StateParseErorrs(errs: NonEmptyList[StateParseError])
      extends ScoreRepositoryError
  case object ScoreNotFoundError extends ScoreRepositoryError

  sealed trait StateParseError
  case class MultiplePossibleStatesFound(states: List[State])
      extends StateParseError
  case object NoStatesFound extends StateParseError
  // case class InvalidScores(p1Score: Int, p2Score: Int) extends StateParseError
  case class InvalidAdvantagePlayer(player: Int) extends StateParseError
  case class InvalidWinPlayer(player: Int) extends StateParseError
  case class InvalidPlayer1Score(p1Raw: Int) extends StateParseError
  case class InvalidPlayer2Score(p2Raw: Int) extends StateParseError

  def fromScoreTableRow(
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
        println(playerInt)
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

  // case class ScoreTableRow2(
  //     id: Long,
  //     isDeuce: Boolean,
  //     playerWithAdvantage: Option[Int],
  //     playerThatWon: Option[Int],
  //     p1Score: Int,
  //     p2Score: String
  // )

  // def getScoreSql2(
  //     id: Long
  // ) = {
  //   sql"SELECT id, isDeuce, playerWithAdvantage, playerThatWon, p1Score, p2Score FROM score WHERE id = $id"
  //     .query[ScoreRepository.ScoreTableRow2]
  // }

  def getScoreSql(
      id: Long
  ): doobie.Query0[ScoreRepository.ScoreTableRow] = {
    sql"SELECT id, isDeuce, playerWithAdvantage, playerThatWon, p1Score, p2Score FROM score WHERE id = $id".query
  }

  def updateSql(id: Long, st: ScoreTableRow): doobie.Update0 = {
    sql"""
      UPDATE score
      SET isDeuce = ${st.isDeuce},
          playerWithAdvantage = ${st.playerWithAdvantage},
          playerThatWon = ${st.playerThatWon},
          p1Score = ${st.p1Score},
          p2Score = ${st.p2Score}
      WHERE id = $id
    """.update
  }

  private def getScore_(
      id: Long
  ): ConnectionIO[Either[ScoreRepositoryError, State]] = {
    getScoreSql(id).option
      .map {
        case None => Left(ScoreNotFoundError)
        case Some(score) => {
          ScoreRepository.fromScoreTableRow(score) match {
            case Valid(s)      => Right(s)
            case Invalid(errs) => Left(StateParseErorrs(errs))
          }
        }
      }
  }
}

class ScoreRepository(transactor: Transactor[IO]) {
  def getScore(
      id: Long
  ): IO[Either[ScoreRepository.ScoreRepositoryError, State]] = {
    ScoreRepository.getScore_(id).transact(transactor)
  }

  def createScore(): IO[Long] = {
    sql"INSERT INTO score DEFAULT VALUES".update
      .withUniqueGeneratedKeys[Long]("id")
      .transact(transactor)
  }

  def updateScore(
      id: Long,
      newState: State
  ): IO[Either[ScoreRepository.ScoreRepositoryError, State]] = {
    val scoreTableRow = ScoreRepository.toScoreTableRow(id, newState)

    ScoreRepository
      .updateSql(id, scoreTableRow)
      .withUniqueGeneratedKeys[Long]("id")
      .flatMap(gameId => ScoreRepository.getScore_(gameId))
      .transact(transactor)

  }
}
