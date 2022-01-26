package repository

import cats.effect.IO
import doobie.util.transactor.Transactor
import fs2.Stream
import model.{State, Score, ScoreNotFoundError, NormalScoring}
import doobie._
import doobie.implicits._

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

  sealed trait StateParseError
  case class MultipleStatesPossibleFound(states: List[State])
      extends StateParseError
  case object NoStatesFound extends StateParseError
  // case class InvalidScores(p1Score: Int, p2Score: Int) extends StateParseError

  def fromScoreTableRow(st: ScoreTableRow): Either[StateParseError, State] = {
    val optDeuce = if (st.isDeuce) Some(model.Deuce) else None

    val optAdvantage: Option[model.Advantage] =
      st.playerWithAdvantage
        .flatMap(model.intToPlayer)
        .map(model.Advantage)

    val optWin: Option[model.Win] =
      st.playerThatWon
        .flatMap(model.intToPlayer)
        .map(model.Win)

    val optNormalScore: Option[model.NormalScoring] =
      (model.intToScore(st.p1Score), model.intToScore(st.p2Score)) match {
        case (Some(p1Score), Some(p2Score)) =>
          Some(model.NormalScoring((p1Score, p2Score)))

        case _ =>
          // return Left(InvalidScores(st.p1Score, st.p2Score))
          None
      }

    (optDeuce, optAdvantage, optWin) match {
      case (Some(deuce), None, None)     => Right(deuce)
      case (None, Some(advantage), None) => Right(advantage)
      case (None, None, Some(win))       => Right(win)
      case (_) => {

        val possibleStates: List[Option[State]] =
          List(optDeuce, optAdvantage, optWin, optNormalScore)

        val validStates: List[State] = possibleStates.flatten

        validStates match {
          case (normalScore: NormalScoring) :: Nil => Right(normalScore)
          case Nil => Left(NoStatesFound)
          case (_) => Left(MultipleStatesPossibleFound(validStates))
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
  ): ConnectionIO[Either[ScoreNotFoundError.type, State]] = {
    getScoreSql(id).option
      .map {
        case None => Left(ScoreNotFoundError)
        case Some(score) => {
          // TODO handle either
          ScoreRepository.fromScoreTableRow(score).toOption match {
            case Some(s) => Right(s)
            case None    => Left(ScoreNotFoundError)
          }
        }
      }
  }
}

class ScoreRepository(transactor: Transactor[IO]) {

  def getScore(id: Long): IO[Either[ScoreNotFoundError.type, State]] = {
    ScoreRepository.getScore_(id).transact(transactor)
  }

  def createScore(): IO[Long] = {
    sql"INSERT INTO score DEFAULT VALUES".update
      .withUniqueGeneratedKeys[Long]("id")
      .transact(transactor)
  }

  def updateScore(id: Long, newState: State): IO[Option[State]] = {
    val scoreTableRow = ScoreRepository.toScoreTableRow(id, newState)

    ScoreRepository
      .updateSql(id, scoreTableRow)
      .withUniqueGeneratedKeys[Long]("id")
      .flatMap(gameId => ScoreRepository.getScore_(gameId))
      .map(_.toOption)
      .transact(transactor)

  }
}
