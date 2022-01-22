package service

import cats.effect.IO
import fs2.Stream
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.{Decoder, Encoder}
import model.{Player, Score, State, ScoreNotFoundError }
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.{Location, `Content-Type`}
import org.http4s.{HttpRoutes, MediaType, Uri}
import repository.ScoreRepository
class ScoreService(repository: ScoreRepository) extends Http4sDsl[IO] {

  case class ScoreRequest(
      gameId: Int,
      player: Player
  )

  case class ScoreResponseJson(
      isDeuce: Boolean,
      playerWithAdvantage: Option[Int],
      playerThatWon: Option[Int],
      p1Score: Int,
      p2Score: Int
  )

  def toScoreResponseJson(state: State): ScoreResponseJson = {
    state match {
      case model.NormalScoring((p1Score, p2Score)) => {
        ScoreResponseJson(
          isDeuce = false,
          playerWithAdvantage = None,
          playerThatWon = None,
          p1Score = model.scoreToInt(p1Score),
          p2Score = model.scoreToInt(p2Score)
        )
      }
      case model.Deuce => {
        ScoreResponseJson(
          isDeuce = true,
          playerWithAdvantage = None,
          playerThatWon = None,
          p1Score = 3,
          p2Score = 3
        )
      }
      case model.Advantage(player) => {
        ScoreResponseJson(
          isDeuce = true,
          playerWithAdvantage = Some(model.playerToInt(player)),
          playerThatWon = None,
          p1Score = 3,
          p2Score = 3
        )
      }
      case model.Win(player) => {
        ScoreResponseJson(
          isDeuce = false,
          playerWithAdvantage = None,
          playerThatWon = Some(model.playerToInt(player)),
          p1Score = 0,
          p2Score = 0
        )
      }
    }

  }

  val routes = HttpRoutes.of[IO] {
    case GET -> Root / "scores" / LongVar(gameId) =>
      for {
        getResult <- repository.getScore(gameId)
        response <- scoreResult(getResult)
      } yield response

    // case req @ POST -> Root / "scored" / LongVar(gameId) / "player" / LongVar(player) =>
    //   for {
    //     score <- req.decodeJson[Score]
    //     createdScore <- repository.createScore(score)
    //     response <- Created(
    //       createdScore.asJson,
    //       Location(Uri.unsafeFromString(s"/scores/${createdScore.id.get}"))
    //     )
    //   } yield response

  }

  private def scoreResult(result: Either[ScoreNotFoundError.type, State]) = {
    result match {
      case Left(ScoreNotFoundError) => NotFound()
      case Right(state)             => Ok(toScoreResponseJson(state).asJson)
    }
  }
}
