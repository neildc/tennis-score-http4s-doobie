import cats.effect.{ExitCode, IO, IOApp}
import model.JsonSchemaa

object ServerApp extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    HttpServer.create()
    //println(JsonSchemaa.print)
    //IO(ExitCode.Success)
  }
}
