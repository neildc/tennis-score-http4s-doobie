package repository

import cats.effect.IO
import doobie.util.transactor.Transactor
import fs2.Stream
import model.{Importance, Score, ScoreNotFoundError}
import doobie._
import doobie.implicits._

class ScoreRepository(transactor: Transactor[IO]) {
  private implicit val importanceMeta: Meta[Importance] = Meta[String].timap(Importance.unsafeFromString)( _.value)

  def getScores: Stream[IO, Score] = {
    sql"SELECT id, description, importance FROM score".query[Score].stream.transact(transactor)
  }

  def getScore(id: Long): IO[Either[ScoreNotFoundError.type, Score]] = {
    sql"SELECT id, description, importance FROM score WHERE id = $id".query[Score].option.transact(transactor).map {
      case Some(score) => Right(score)
      case None => Left(ScoreNotFoundError)
    }
  }

  def createScore(score: Score): IO[Score] = {
    sql"INSERT INTO score (description, importance) VALUES (${score.description}, ${score.importance})".update.withUniqueGeneratedKeys[Long]("id").transact(transactor).map { id =>
      score.copy(id = Some(id))
    }
  }

  def deleteScore(id: Long): IO[Either[ScoreNotFoundError.type, Unit]] = {
    sql"DELETE FROM score WHERE id = $id".update.run.transact(transactor).map { affectedRows =>
      if (affectedRows == 1) {
        Right(())
      } else {
        Left(ScoreNotFoundError)
      }
    }
  }

  def updateScore(id: Long, score: Score): IO[Either[ScoreNotFoundError.type, Score]] = {
    sql"UPDATE score SET description = ${score.description}, importance = ${score.importance} WHERE id = $id".update.run.transact(transactor).map { affectedRows =>
      if (affectedRows == 1) {
        Right(score.copy(id = Some(id)))
      } else {
        Left(ScoreNotFoundError)
      }
    }
  }
}
