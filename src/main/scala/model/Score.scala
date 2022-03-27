package model

import io.circe.{Decoder}
import cats.syntax.functor._
import io.circe.syntax._
import io.circe._, io.circe.generic.semiauto._

// State -> ADT (Algebraic Data Type)
sealed trait State
final case class NormalScoring(score: (Score, Score)) extends State
final case object Deuce extends State
final case class Advantage(advantage: Player) extends State
// TODO: What was the score when they won?
final case class Win(win: Player) extends State

object State {
  implicit val decodeNormalScoring: Decoder[NormalScoring] = ???
  implicit val decodeAdvantage: Decoder[Advantage] = ???
  implicit val decodeWin: Decoder[Win] = ???

  val decodeDeuce: Decoder[State] = ???

  implicit val decodeState: Decoder[State] = ???
}

sealed trait Score
final case object ScoreLove extends Score
final case object Score15 extends Score
final case object Score30 extends Score
final case object Score40 extends Score

object Score {

  def scoreToInt(score: Score): Int = ???
  def intToScore(i: Int): Option[Score] = ???
  implicit val decodeScore: Decoder[Score] = ???
}

sealed trait Player
final case object P1 extends Player
final case object P2 extends Player

object Player {

  def intToPlayer(i: Int): Option[Player] = ???
  def playerToInt(player: Player): Int = ???

  implicit val decodePlayer: Decoder[Player] = ???

}

object JsonSchemaa {
  import json._
  import com.github.andyglow.jsonschema.AsCirce._
  import json.schema.Version._

  val a: json.Schema[Advantage] =
    Json.schema[Advantage].toDefinition("advantage")
  val w: json.Schema[Win] = Json.schema[Win].toDefinition("win")
  val p: json.Schema[Player] = Json.schema[Player].toDefinition("player")
  val s: json.Schema[NormalScoring] =
    Json.schema[NormalScoring].toDefinition("score")

  implicit val state: json.Schema[State] = Json.schema[State]

  def print = {
    println(JsonSchemaa.w.asCirce(Draft04()).toString)
    println(JsonSchemaa.s.asCirce(Draft04()).toString)
    println(JsonSchemaa.a.asCirce(Draft04()).toString)
    println(oneOf("state", List("win", "score", "advantage")))
  }

  def oneOf(name: String, refs: List[String]): String = {
    def toRef(r: String) = s"""{ "ref" : #/link/to/$r }"""

    s"""
      {
        "$$schema" : "http://json-schema.org/draft-04/schema#",
        "oneOf" : ${refs.map(toRef).asJson}
      }
    """
  }
}
