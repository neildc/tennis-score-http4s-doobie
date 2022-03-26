package repository

import cats.data.EitherT
import cats.effect.IO
import doobie.util.transactor.Transactor
import fs2.Stream
import model.{Score, State}
import cats.data.Validated._
import doobie._
import doobie.implicits._
import cats.data.NonEmptyList
import model.db.{ScoreTableRow, ScoreTableRowWithoutId, StateParseError}

object ScoreRepository {
  sealed trait ScoreRepositoryError
  case class StateParseErorrs(errs: NonEmptyList[StateParseError])
      extends ScoreRepositoryError
  case object ScoreNotFoundError extends ScoreRepositoryError


  def getScoreSql(
      id: Long
  ): doobie.Query0[ScoreTableRowWithoutId] = {
    val cols = Fragment.const(ScoreTableRowWithoutId.columns.mkString(","))

    fr"SELECT $cols FROM score WHERE id = $id".query
  }

  def updateSql(id: Long, st: ScoreTableRowWithoutId): doobie.Update0 = {
    val stuff = ScoreTableRowWithoutId.columns
      .map(c => s"$c = ?")
      .mkString(",")

    val sql = s"""
          UPDATE score
          SET $stuff
          WHERE id = $id
    """
    Update[ScoreTableRowWithoutId](sql).toUpdate0(st)
  }
  def insertSql(st: ScoreTableRowWithoutId): doobie.Update0 = {
      val cols = ScoreTableRowWithoutId.columns.mkString(",")
      val questionMarks = ScoreTableRowWithoutId.columns.map(_ => "?").mkString(",")

      val sql = s"INSERT INTO score($cols) VALUES ($questionMarks)"
      Update[ScoreTableRowWithoutId](sql).toUpdate0(st)
    }

  private def getScore_(
      id: Long
  ): ConnectionIO[Either[ScoreRepositoryError, model.State]] = {
    getScoreSql(id).option
      .map {
        case None => Left(ScoreNotFoundError)
        case Some(score) => {
          ScoreTableRowWithoutId.toState(score) match {
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

  def insertScore(state: State): IO[Either[ScoreRepository.ScoreRepositoryError, (Long, model.State)]] = {
      val row = ScoreTableRowWithoutId.fromState(state)

    ScoreRepository.insertSql(row)
      .withUniqueGeneratedKeys[Long]("id")
      .flatMap(gameId => EitherT(ScoreRepository.getScore_(gameId))
                 .map(s => (gameId, s)).value)
      .transact(transactor)
    }

  def updateScore(
      id: Long,
      newState: State
  ): IO[Either[ScoreRepository.ScoreRepositoryError, model.State]] = {
    val scoreTableRow = ScoreTableRowWithoutId.fromState(newState)

    ScoreRepository
      .updateSql(id, scoreTableRow)
      .withUniqueGeneratedKeys[Long]("id")
      .flatMap(gameId => ScoreRepository.getScore_(gameId))
      .transact(transactor)

  }
}
