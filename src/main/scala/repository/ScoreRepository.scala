package repository

import cats.effect.IO
import doobie.util.transactor.Transactor
import fs2.Stream
import model.{State, Score, ScoreNotFoundError}
import doobie._
import doobie.implicits._

object ScoreRepository {
  case class ScoreTable(
      id: Long,
      isDeuce: Boolean,
      playerWithAdvantage: Option[Int],
      playerThatWon: Option[Int],
      p1Score: Int,
      p2Score: Int
  )

  def toScoreTable(id: Long, state: State): ScoreTable = {
    state match {
      case model.NormalScoring((p1Score, p2Score)) => {
        ScoreTable(
          id = id,
          isDeuce = false,
          playerWithAdvantage = None,
          playerThatWon = None,
          p1Score = model.scoreToInt(p1Score),
          p2Score = model.scoreToInt(p2Score)
        )
      }
      case model.Deuce => {
        ScoreTable(
          id = id,
          isDeuce = true,
          playerWithAdvantage = None,
          playerThatWon = None,
          p1Score = 3,
          p2Score = 3
        )
      }
      case model.Advantage(player) => {
        ScoreTable(
          id = id,
          isDeuce = false,
          playerWithAdvantage = Some(model.playerToInt(player)),
          playerThatWon = None,
          p1Score = 3,
          p2Score = 3
        )
      }
      case model.Win(player) => {
        ScoreTable(
          id = id,
          isDeuce = false,
          playerWithAdvantage = None,
          playerThatWon = Some(model.playerToInt(player)),
          p1Score = 0,
          p2Score = 0
        )
      }
    }

  }

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
}

class ScoreRepository(transactor: Transactor[IO]) {
  def getScore_(
      id: Long
  ): ConnectionIO[Either[ScoreNotFoundError.type, State]] = {
    sql"SELECT id, isDeuce, playerWithAdvantage, playerThatWon, p1Score, p2Score FROM score WHERE id = $id"
      .query[ScoreRepository.ScoreTable]
      .option
      .map {
        case None => Left(ScoreNotFoundError)
        case Some(score) => {
          ScoreRepository.fromScoreTable(score) match {
            case Some(s) => Right(s)
            case None    => Left(ScoreNotFoundError)
          }
        }
      }
  }

  def getScore(id: Long): IO[Either[ScoreNotFoundError.type, State]] = {
    getScore_(id).transact(transactor)
  }

  def createScore(): IO[Long] = {
    sql"INSERT INTO score DEFAULT VALUES".update
      .withUniqueGeneratedKeys[Long]("id")
      .transact(transactor)
  }

  def updateScore(id: Long, newState: State): IO[Option[State]] = {
    val st: ScoreRepository.ScoreTable =
      ScoreRepository.toScoreTable(id, newState)

    val update: ConnectionIO[Long] =
      sql"""
        UPDATE score
        SET isDeuce = ${st.isDeuce},
            playerWithAdvantage = ${st.playerWithAdvantage},
            playerThatWon = ${st.playerThatWon},
            p1Score = ${st.p1Score},
            p2Score = ${st.p2Score}
        WHERE id = $id"""
        .update
        .withUniqueGeneratedKeys[Long]("id")

    update
      .flatMap(gameId => getScore_(gameId))
      .map(_.toOption)
      .transact(transactor)

  }
}
