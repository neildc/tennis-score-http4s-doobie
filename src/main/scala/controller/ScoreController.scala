package controller

import cats.effect.IO
import fs2.Stream
import io.circe.generic.auto._
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.{Decoder, Encoder}
import model.{Score, State, Player}
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.{Location, `Content-Type`}
import org.http4s.{HttpRoutes, MediaType, Uri, Request}
import repository.ScoreRepository
import org.http4s.Status
import model.db.{ScoreTableRow, ScoreTableRowWithoutId}
import service.ScoreService

import ru.tinkoff.phobos.decoding._
import ru.tinkoff.phobos.encoding._
import ru.tinkoff.phobos.syntax._
import ru.tinkoff.phobos.derivation.semiauto._

class ScoreController(repository: ScoreRepository) extends Http4sDsl[IO] {

  val routes = HttpRoutes.of[IO] {
    case GET -> Root / "scores" / LongVar(gameId) =>
      for {
        scoreResult <- repository.getScore(gameId)
        response <- scoreResult match {
          case Left(scoreRepoErr) => fromScoreRepositoryError(scoreRepoErr)
          case Right(state) => Ok(toScoreResponseJson(gameId, state).asJson)
        }
      } yield response

    case GET -> Root / "scoresString" / LongVar(gameId) =>
      for {
        scoreResult <- repository.getScore(gameId)
        response <- scoreResult match {
          case Left(scoreRepoErr) => fromScoreRepositoryError(scoreRepoErr)
          case Right(state)       => Ok(toScoreResponseStringJson(state).asJson)
        }
      } yield response

    case GET -> Root / "scoresXML" / LongVar(gameId) =>
      for {
        scoreResult <- repository.getScore(gameId)
        response <- scoreResult match {
          case Left(scoreRepoErr) => fromScoreRepositoryError(scoreRepoErr)
          case Right(state) =>
            Ok(XmlEncoder[ScoreXml].encode(toScoreXml(gameId, state)))
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

    case req @ POST -> Root / "loadGame" =>
      for {
        scoreRequestJson <- req.attemptAs[ScoreTableRowWithoutId].value

        resp <- scoreRequestJson match {
          case Right(scoreRequest) =>
            ScoreTableRowWithoutId.toState(scoreRequest).toEither match {
              case Right(state)     => insertScore(state)
              case Left(parseError) => BadRequest(parseError.toString)
            }
          case Left(err) => {
            val sampleJson =
              ScoreTableRowWithoutId.fromState(ScoreService.startGame()).asJson
            BadRequest(s"Invalid body, expected something like:\n $sampleJson")
          }
        }
      } yield (resp)

    case req @ POST -> Root / "loadGameOneOf" =>
      for {
        oneOfStateJson <- req.attemptAs[State].value

        resp <- oneOfStateJson match {
          case Right(state) => insertScore(state)
          case Left(err) => {
            //val sampleJson = ScoreTableRowWithoutId.fromState(ScoreService.startGame()).asJson
            //BadRequest(s"Invalid body, expected something like:\n $sampleJson")
            BadRequest(err.toString)
          }
        }
      } yield (resp)
  }

  implicit val d2 = accumulatingJsonOf[IO, ScoreTableRowWithoutId]
  implicit val d3 = accumulatingJsonOf[IO, State]

  case class CreateResponseJson(gameId: Long)

  implicit val decoder = accumulatingJsonOf[IO, ScoreRequest]
  case class ScoreRequest(gameId: Long, player: Int)

  def updateScore(sr: ScoreRequest) = {
    Player.intToPlayer(sr.player) match {
      case None => BadRequest("Player invalid")
      case Some(player) =>
        for {
          stateResult <- repository.getScore(sr.gameId)
          _ <- IO(println(stateResult))
          updated <- stateResult match {
            case Left(scoreRepoErr) => fromScoreRepositoryError(scoreRepoErr)
            case Right(state) =>
              repository
                .updateScore(sr.gameId, ScoreService.score(player)(state))
                .flatMap(dbResult =>
                  dbResult match {
                    case Left(scoreRepoErr) =>
                      fromScoreRepositoryError(scoreRepoErr)
                    case Right(newState) => {
                      println(newState)
                      Ok(toScoreResponseJson(sr.gameId, newState).asJson)
                    }
                  }
                )
          }

        } yield updated
    }
  }

  def insertScore(state: State) =
    repository
      .insertScore(state)
      .flatMap(dbResult =>
        dbResult match {
          case Left(scoreRepoErr) =>
            fromScoreRepositoryError(scoreRepoErr)
          case Right((id, newState)) => {
            println(newState)
            Ok(toScoreResponseJson(id, newState).asJson)
          }
        }
      )

  def fromScoreRepositoryError(err: ScoreRepository.ScoreRepositoryError) = {
    err match {
      case ScoreRepository.ScoreNotFoundError => BadRequest("Game Not found")
      case ScoreRepository.StateParseErorrs(errs) =>
        InternalServerError(s"state errors: ${errs}")
    }
  }

  type ScoreResponseJson = ScoreTableRow

  def toScoreResponseJson(id: Long, s: State): ScoreResponseJson =
    ScoreTableRow(id, ScoreTableRowWithoutId.fromState(s))

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

  case class ScoresNested(p1Score: Int, p2Score: Int)
  implicit val scoresNestedElEnc: ElementEncoder[ScoresNested] =
    deriveElementEncoder

  case class EmptyTagHack[A](opt: Option[A])

  implicit def eee[A]: ElementEncoder[EmptyTagHack[A]] =
    ElementEncoder.stringEncoder.contramap(e => emptyTagHack(e.opt))

  def emptyTagHack[A](opt: Option[A]): String =
    opt.map(_.toString()).getOrElse("")

  case class ScoreXml(
      @attr id: Long,
      isDeuce: Boolean,
      playerWithAdvantage: EmptyTagHack[Int],
      playerThatWon: EmptyTagHack[Int],
      scores: ScoresNested
  )
  implicit val scoreXmlEnc: XmlEncoder[ScoreXml] = deriveXmlEncoder("AAAA")

  def toScoreXml(id: Long, state: State): ScoreXml = {
    val s = ScoreTableRowWithoutId.fromState(state)

    ScoreXml(
      id = id,
      isDeuce = s.isDeuce,
      playerWithAdvantage = EmptyTagHack(s.playerWithAdvantage),
      playerThatWon = EmptyTagHack(s.playerThatWon),
      scores = ScoresNested(s.p1Score, s.p2Score)
    )
  }

}
