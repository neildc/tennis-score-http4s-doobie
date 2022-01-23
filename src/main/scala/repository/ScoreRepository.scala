package repository

import cats.effect.IO
import doobie.util.transactor.Transactor
import fs2.Stream
import model.{State, Score, ScoreNotFoundError}
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
    state match {
      case model.NormalScoring((p1Score, p2Score)) => {
        ScoreTableRow(
          id = id,
          isDeuce = false,
          playerWithAdvantage = None,
          playerThatWon = None,
          p1Score = model.scoreToInt(p1Score),
          p2Score = model.scoreToInt(p2Score)
        )
      }
      case model.Deuce => {
        ScoreTableRow(
          id = id,
          isDeuce = true,
          playerWithAdvantage = None,
          playerThatWon = None,
          p1Score = 3,
          p2Score = 3
        )
      }
      case model.Advantage(player) => {
        ScoreTableRow(
          id = id,
          isDeuce = false,
          playerWithAdvantage = Some(model.playerToInt(player)),
          playerThatWon = None,
          p1Score = 3,
          p2Score = 3
        )
      }
      case model.Win(player) => {
        ScoreTableRow(
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

  def fromScoreTableRow(st: ScoreTableRow): Option[State] = {
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
    sql"SELECT id, isDeuce, playerWithAdvantage, playerThatWon, p1Score, p2Score FROM score WHERE id = $id"
      .query
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
          ScoreRepository.fromScoreTableRow(score) match {
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
