package service

import cats.effect.IO
import fs2.Stream
import io.circe.generic.auto._
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.{Decoder, Encoder}
import model.{Player, Score, State, ScoreNotFoundError}
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.{Location, `Content-Type`}
import org.http4s.{HttpRoutes, MediaType, Uri, Request}
import repository.ScoreRepository
import org.http4s.Status

import ru.tinkoff.phobos.decoding._
import ru.tinkoff.phobos.encoding._
import ru.tinkoff.phobos.syntax._
import ru.tinkoff.phobos.derivation.semiauto._

class ScoreService(repository: ScoreRepository) extends Http4sDsl[IO] {

  // implicit val decoder = jsonOf[IO, ScoreRequest]
  implicit val decoder = accumulatingJsonOf[IO, ScoreRequest]
  // implicit val fooDecoder: Decoder[ScoreRequest] = deriveDecoder
  case class ScoreRequest(gameId: Long, player: Int)

  case class CreateResponseJson(gameId: Long)

  type ScoreResponseJson = ScoreRepository.ScoreTableRow

  def toScoreResponseJson(id: Long, s: State): ScoreResponseJson =
    ScoreRepository.toScoreTableRow(id, s)

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

      for {
        scoreResult <- repository.getScore(gameId)
        response <- scoreResult match {
          case Left(ScoreNotFoundError) => NotFound("Score not found")
          case Right(state) => Ok(toScoreResponseStringJson(state).asJson)
        }
      } yield response

    case GET -> Root / "scoresXML" / LongVar(gameId) =>
      handleXml(gameId)

    case req @ POST -> Root / "startGame" =>
      for {
        getResult <- repository.createScore()
        response <- Ok(CreateResponseJson(getResult).asJson)
      } yield response

    case req @ POST -> Root / "scored" / LongVar(gameId) / "player" / IntVar(
          playerInt
        ) =>
      updateScore(ScoreRequest(gameId, playerInt))

    case req @ POST -> Root / "scored" =>
      for {
        scoreRequestJson <- req.attemptAs[ScoreRequest].value

        resp <- scoreRequestJson match {
          case Right(scoreRequest) => updateScore(scoreRequest)
          case Left(err) => {
            val sampleJson = ScoreRequest(1, 2).asJson
            BadRequest(s"Invalid body, expected something like:\n $sampleJson")
          }
        }

      } yield (resp)

  }
  def updateScore(sr: ScoreRequest) = {
    model.intToPlayer(sr.player) match {
      case None => BadRequest("Player invalid")
      case Some(player) =>
        for {
          stateResult <- repository.getScore(sr.gameId)
          _ <- IO(println(stateResult))
          updated <- stateResult match {
            case Left(ScoreNotFoundError) => BadRequest("Not found game")
            case Right(state) =>
              repository
                .updateScore(sr.gameId, model.score(player)(state))
                .flatMap(k =>
                  k match {
                    case None => InternalServerError("Failed to update")
                    case Some(newState) => {
                      println(newState)
                      Ok(toScoreResponseJson(sr.gameId, newState).asJson)
                    }
                  }
                )

          }

        } yield updated
    }
  }

  case class ScoresNested(p1Score: Int, p2Score: Int)
  implicit val scoresNestedElEnc: ElementEncoder[ScoresNested] = deriveElementEncoder

  case class ScoreXml(
      @attr id: Long,
      isDeuce: Boolean,
      playerWithAdvantage: String,
      playerThatWon: String,
      scores: ScoresNested
  )
  implicit val scoreXmlEnc: XmlEncoder[ScoreXml] = deriveXmlEncoder("AAAA")

  def emptyTagIfNone[A](opt: Option[A]) : String = opt.map(_.toString()).getOrElse("")

  def toScoreXml(id: Long, state: State): ScoreXml = {
    val s = ScoreRepository.toScoreTableRow(id, state)

    ScoreXml(
      id = s.id,
      isDeuce = s.isDeuce,
      playerWithAdvantage = emptyTagIfNone(s.playerWithAdvantage),
      playerThatWon = emptyTagIfNone(s.playerThatWon),
      scores = ScoresNested(s.p1Score, s.p2Score)
    )
  }

  def handleXml(gameId: Long) = {

    for {
      scoreResult <- repository.getScore(gameId)
      response <- scoreResult match {
        case Left(ScoreNotFoundError) => NotFound("Score not found")
        case Right(state) =>
          Ok(XmlEncoder[ScoreXml].encode(toScoreXml(gameId, state)))
      }
    } yield response

  }

}
