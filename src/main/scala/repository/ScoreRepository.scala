package repository

import cats.effect.IO
import doobie.util.transactor.Transactor
import fs2.Stream
import model.{Score, State}
import cats.data.Validated._
import doobie._
import doobie.implicits._
import cats.data.NonEmptyList
import model.db.ScoreTableRow

object ScoreRepository {
  sealed trait ScoreRepositoryError
  case class StateParseErorrs(errs: NonEmptyList[ScoreTableRow.StateParseError])
      extends ScoreRepositoryError
  case object ScoreNotFoundError extends ScoreRepositoryError


  def getScoreSql(
      id: Long
  ): doobie.Query0[ScoreTableRow] = {
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
  ): ConnectionIO[Either[ScoreRepositoryError, model.State]] = {
    getScoreSql(id).option
      .map {
        case None => Left(ScoreNotFoundError)
        case Some(score) => {
          ScoreTableRow.toState(score) match {
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
  ): IO[Either[ScoreRepository.ScoreRepositoryError, model.State]] = {
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
  ): IO[Either[ScoreRepository.ScoreRepositoryError, model.State]] = {
    val scoreTableRow = ScoreTableRow.fromState(id, newState)

    ScoreRepository
      .updateSql(id, scoreTableRow)
      .withUniqueGeneratedKeys[Long]("id")
      .flatMap(gameId => ScoreRepository.getScore_(gameId))
      .transact(transactor)

  }
}
