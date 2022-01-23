package service

import cats.effect.IO
import fs2.Stream
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.{Decoder, Encoder}
import model.{Player, Score, State, ScoreNotFoundError}
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.{Location, `Content-Type`}
import org.http4s.{HttpRoutes, MediaType, Uri}
import repository.ScoreRepository
import org.http4s.Status

class ScoreService(repository: ScoreRepository) extends Http4sDsl[IO] {

  case class ScoreRequest(gameId: Long, player: Player)

  case class CreateResponseJson(gameId: Long)

  type ScoreResponseJson = ScoreRepository.ScoreTableRow

  def toScoreResponseJson(id: Long, s: State): ScoreResponseJson =
    ScoreRepository.toScoreTableRow(id, s)

  case class ScoreResponseStringJson(message: String)

  def toScoreResponseStringJson(state: State): ScoreResponseStringJson = {
    ScoreResponseStringJson(
      state match {
        case model.NormalScoring((p1Score, p2Score)) =>
          s"P1: $p1Score, P2: $p2Score"
        case model.Deuce             => "deuce"
        case model.Advantage(player) => s"advantage ${player}"
        case model.Win(player)       => s"${player} won"
      }
    )
  }

  val routes = HttpRoutes.of[IO] {
    case GET -> Root / "scores" / LongVar(gameId) =>
      for {
        scoreResult <- repository.getScore(gameId)
        response <- scoreResult match {
          case Left(ScoreNotFoundError) => NotFound("Score not found")
          case Right(state) => Ok(toScoreResponseJson(gameId, state).asJson)
        }
      } yield response

    case GET -> Root / "scoresString" / LongVar(gameId) =>
      for {
        scoreResult <- repository.getScore(gameId)
        response <- scoreResult match {
          case Left(ScoreNotFoundError) => NotFound("Score not found")
          case Right(state) => Ok(toScoreResponseStringJson(state).asJson)
        }
      } yield response

    case req @ POST -> Root / "startGame" =>
      for {
        getResult <- repository.createScore()
        response <- Ok(CreateResponseJson(getResult).asJson)
      } yield response

    case req @ POST -> Root / "scored" / LongVar(gameId) / "player" / IntVar(
          playerInt
        ) =>
      model.intToPlayer(playerInt) match {
        case None => BadRequest("Player invalid")
        case Some(player) =>
          for {
            stateResult <- repository.getScore(gameId)
            _ <- IO(println(stateResult))
            updated <- stateResult match {
              case Left(ScoreNotFoundError) => BadRequest("Not found game")
              case Right(state) =>
                repository
                  .updateScore(gameId, model.score(player)(state))
                  .flatMap(k =>
                    k match {
                      case None => InternalServerError("Failed to update")
                      case Some(newState) => {
                        println(newState)
                        Ok(toScoreResponseJson(gameId, newState).asJson)
                      }
                    }
                  )

            }

          } yield updated
      }

  }

}
