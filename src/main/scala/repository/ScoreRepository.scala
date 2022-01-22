package repository

import cats.effect.IO
import doobie.util.transactor.Transactor
import fs2.Stream
import model.{State, Score, ScoreNotFoundError}
import doobie._
import doobie.implicits._

class ScoreRepository(transactor: Transactor[IO]) {
  case class ScoreTable(
      id: Int,
      isDeuce: Boolean,
      playerWithAdvantage: Option[Int],
      playerThatWon: Option[Int],
      p1Score: Int,
      p2Score: Int
  )

  def fromScoreTable(st: ScoreTable): Option[State] = {
    if (st.isDeuce) { return Some(model.Deuce) }

    st.playerWithAdvantage.flatMap(model.intToPlayer) match {
      case Some(playerId) => { return Some(model.Advantage(playerId)) }
      case None => {
        st.playerThatWon.flatMap(model.intToPlayer) match {
          case Some(playerId) => { return Some(model.Win(playerId)) }
          case None =>
            (model.intToScore(st.p1Score), model.intToScore(st.p2Score)) match {
              case (Some(p1Score), Some(p2Score)) => {
                return Some(model.NormalScoring((p1Score, p2Score)))
              }
              case _ => return { None }
            }
        }

      }
    }
  }

  def getScore(id: Long): IO[Either[ScoreNotFoundError.type, State]] = {
    sql"SELECT id, isDeuce, playerWithAdvantage, playerThatWon, p1Score, p2Score FROM score WHERE id = $id"
      .query[ScoreTable]
      .option
      .transact(transactor)
      .map {
        case Some(score) => {
          fromScoreTable(score) match {
            case Some(s) => Right(s)
            case None    => Left(ScoreNotFoundError)
          }
        }
        case None => Left(ScoreNotFoundError)
      }
  }

  // def createScore(score: Score): IO[Score] = {
  //   sql"INSERT INTO score (description, importance) VALUES (${score.description}, ${score.importance})".update.withUniqueGeneratedKeys[Long]("id").transact(transactor).map { id =>
  //     score.copy(id = Some(id))
  //   }
  // }

  // def deleteScore(id: Long): IO[Either[ScoreNotFoundError.type, Unit]] = {
  //   sql"DELETE FROM score WHERE id = $id".update.run.transact(transactor).map { affectedRows =>
  //     if (affectedRows == 1) {
  //       Right(())
  //     } else {
  //       Left(ScoreNotFoundError)
  //     }
  //   }
  // }

  // def updateScore(id: Long, score: Score): IO[Either[ScoreNotFoundError.type, Score]] = {
  //   sql"UPDATE score SET description = ${score.description}, importance = ${score.importance} WHERE id = $id".update.run.transact(transactor).map { affectedRows =>
  //     if (affectedRows == 1) {
  //       Right(score.copy(id = Some(id)))
  //     } else {
  //       Left(ScoreNotFoundError)
  //     }
  //   }
  // }
}
