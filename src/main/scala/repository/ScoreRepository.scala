package repository

import cats.data.EitherT
import cats.effect.IO
import doobie.util.transactor.Transactor
import fs2.Stream
import model.{Score, State}
import cats.data.Validated._
import doobie._
import doobie.implicits._
import cats.implicits._
import cats.data.NonEmptyList
import model.db.{ScoreTableRow, ScoreTableRowWithoutId, StateParseError}

object ScoreRepository {
  sealed trait ScoreRepositoryError
  case class StateParseErorrs(errs: NonEmptyList[StateParseError])
      extends ScoreRepositoryError
  case object ScoreNotFoundError extends ScoreRepositoryError
  case class UpdateFailed(err: Throwable) extends ScoreRepositoryError
  case class InsertFailed(err: Throwable) extends ScoreRepositoryError

  def getScoreSql(
      id: Long
  ): doobie.Query0[ScoreTableRowWithoutId] = ???

  def updateSql(id: Long, st: ScoreTableRowWithoutId): doobie.Update0 = ???
  def insertSql(st: ScoreTableRowWithoutId): doobie.Update0 = ???

  private def getScore_(
      id: Long
  ): ConnectionIO[Either[ScoreRepositoryError, model.State]] = ???
}

class ScoreRepository(transactor: Transactor[IO]) {
  def getScore(
      id: Long
  ): IO[Either[ScoreRepository.ScoreRepositoryError, model.State]] = ???

  def createScore(): IO[Long] =  ???

  def insertScore(
      state: State
  ): IO[Either[ScoreRepository.InsertFailed, (Long, model.State)]] = ???

  def updateScore(
      id: Long,
      newState: State
  ): IO[Either[ScoreRepository.UpdateFailed, model.State]] = ???
}
