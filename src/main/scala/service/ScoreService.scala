package service

import cats.effect.IO
import fs2.Stream
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.{Decoder, Encoder}
import model.{Importance, Score, ScoreNotFoundError}
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.{Location, `Content-Type`}
import org.http4s.{HttpRoutes, MediaType, Uri}
import repository.ScoreRepository

class ScoreService(repository: ScoreRepository) extends Http4sDsl[IO] {
  private implicit val encodeImportance: Encoder[Importance] = Encoder.encodeString.contramap[Importance](_.value)

  private implicit val decodeImportance: Decoder[Importance] = Decoder.decodeString.map[Importance](Importance.unsafeFromString)

  val routes = HttpRoutes.of[IO] {
    case GET -> Root / "scores" =>
      Ok(Stream("[") ++ repository.getScores.map(_.asJson.noSpaces).intersperse(",") ++ Stream("]"), `Content-Type`(MediaType.application.json))

    case GET -> Root / "scores" / LongVar(id) =>
      for {
        getResult <- repository.getScore(id)
        response <- scoreResult(getResult)
      } yield response

    case req @ POST -> Root / "scores" =>
      for {
        score <- req.decodeJson[Score]
        createdScore <- repository.createScore(score)
        response <- Created(createdScore.asJson, Location(Uri.unsafeFromString(s"/scores/${createdScore.id.get}")))
      } yield response

    case req @ PUT -> Root / "scores" / LongVar(id) =>
      for {
        score <-req.decodeJson[Score]
        updateResult <- repository.updateScore(id, score)
        response <- scoreResult(updateResult)
      } yield response

    case DELETE -> Root / "scores" / LongVar(id) =>
      repository.deleteScore(id).flatMap {
        case Left(ScoreNotFoundError) => NotFound()
        case Right(_) => NoContent()
      }
  }

  private def scoreResult(result: Either[ScoreNotFoundError.type, Score]) = {
    result match {
      case Left(ScoreNotFoundError) => NotFound()
      case Right(score) => Ok(score.asJson)
    }
  }
}
