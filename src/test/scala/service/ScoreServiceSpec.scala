package service

import cats.effect.IO
import fs2.Stream
import io.circe.Json
import io.circe.literal._
import model.{High, Low, Medium, Score}
import org.http4s.circe._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.{Request, Response, Status, Uri, _}
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import repository.ScoreRepository

class ScoreServiceSpec extends AnyWordSpec with MockFactory with Matchers {
  private val repository = stub[ScoreRepository]

  private val service = new ScoreService(repository).routes

  "ScoreService" should {
    "create a score" in {
      val id = 1
      val score = Score(None, "my score", Low)
      (repository.createScore _).when(score).returns(IO.pure(score.copy(id = Some(id))))
      val createJson = json"""
        {
          "description": ${score.description},
          "importance": ${score.importance.value}
        }"""
      val response = serve(Request[IO](POST, uri"/scores").withEntity(createJson))
      response.status shouldBe Status.Created
      response.as[Json].unsafeRunSync() shouldBe json"""
        {
          "id": $id,
          "description": ${score.description},
          "importance": ${score.importance.value}
        }"""
    }

    "update a score" in {
      val id = 1
      val score = Score(None, "updated score", Medium)
      (repository.updateScore _).when(id, score).returns(IO.pure(Right(score.copy(id = Some(id)))))
      val updateJson = json"""
        {
          "description": ${score.description},
          "importance": ${score.importance.value}
        }"""

      val response = serve(Request[IO](PUT, Uri.unsafeFromString(s"/scores/$id")).withEntity(updateJson))
      response.status shouldBe Status.Ok
      response.as[Json].unsafeRunSync() shouldBe json"""
        {
          "id": $id,
          "description": ${score.description},
          "importance": ${score.importance.value}
        }"""
    }

    "return a single score" in {
      val id = 1
      val score = Score(Some(id), "my score", High)
      (repository.getScore _).when(id).returns(IO.pure(Right(score)))

      val response = serve(Request[IO](GET, Uri.unsafeFromString(s"/scores/$id")))
      response.status shouldBe Status.Ok
      response.as[Json].unsafeRunSync() shouldBe json"""
        {
          "id": $id,
          "description": ${score.description},
          "importance": ${score.importance.value}
        }"""
    }

    "return all scores" in {
      val id1 = 1
      val score1 = Score(Some(id1), "my score 1", High)
      val id2 = 2
      val score2 = Score(Some(id2), "my score 2", Medium)
      val scores = Stream(score1, score2)
      (() => repository.getScores ).when().returns(scores)

      val response = serve(Request[IO](GET, uri"/scores"))
      response.status shouldBe Status.Ok
      response.as[Json].unsafeRunSync() shouldBe json"""
        [
         {
           "id": $id1,
           "description": ${score1.description},
           "importance": ${score1.importance.value}
         },
         {
           "id": $id2,
           "description": ${score2.description},
           "importance": ${score2.importance.value}
         }
        ]"""
    }

    "delete a score" in {
      val id = 1
      (repository.deleteScore _).when(id).returns(IO.pure(Right(())))

      val response = serve(Request[IO](DELETE, Uri.unsafeFromString(s"/scores/$id")))
      response.status shouldBe Status.NoContent
    }
  }

  private def serve(request: Request[IO]): Response[IO] = {
    service.orNotFound(request).unsafeRunSync()
  }
}
