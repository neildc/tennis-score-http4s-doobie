lazy val commonSettings = Seq(
  name := "tennis score http4s doobie",
  version := "1.0-SNAPSHOT",
  scalaVersion := "2.13.4",
  scalacOptions ++= Seq(
    "-deprecation",
    "-Xfatal-warnings",
    "-Ywarn-value-discard",
    "-Xlint:missing-interpolator"
  ),
)

lazy val Http4sVersion = "0.21.15"

lazy val DoobieVersion = "0.10.0"

lazy val H2Version = "1.4.200"

lazy val FlywayVersion = "7.5.2"

lazy val CirceVersion = "0.13.0"

lazy val PureConfigVersion = "0.12.3"

lazy val LogbackVersion = "1.2.3"

lazy val Specs2Version  = "4.10.6"

lazy val ScalaJsonSchemaVersion = "0.7.8"

lazy val root = (project in file("."))
  .configs(IntegrationTest)
  .settings(
    commonSettings,
    Defaults.itSettings,
    libraryDependencies ++= Seq(
      "org.http4s"            %% "http4s-blaze-server"  % Http4sVersion,
      "org.http4s"            %% "http4s-circe"         % Http4sVersion,
      "org.http4s"            %% "http4s-dsl"           % Http4sVersion,
      "org.http4s"            %% "http4s-blaze-client"  % Http4sVersion     % "it,test",

      "org.tpolecat"          %% "doobie-core"          % DoobieVersion,
      "org.tpolecat"          %% "doobie-h2"            % DoobieVersion,
      "org.tpolecat"          %% "doobie-hikari"        % DoobieVersion,
      "org.tpolecat"          %% "doobie-specs2"        % DoobieVersion % "test",

      "com.h2database"        %  "h2"                   % H2Version,

      "org.flywaydb"          %  "flyway-core"          % FlywayVersion,

      "io.circe"              %% "circe-generic"        % CirceVersion,
      "io.circe"              %% "circe-literal"        % CirceVersion,
      "io.circe"              %% "circe-optics"         % CirceVersion      % "it",

      "com.github.pureconfig" %% "pureconfig"             % PureConfigVersion,
      "com.github.pureconfig" %% "pureconfig-cats-effect" % PureConfigVersion,

      "ch.qos.logback"        %  "logback-classic"      % LogbackVersion,

      "org.specs2"            %% "specs2-core"        % Specs2Version % "test",

      "ru.tinkoff" %% "phobos-core" % "0.13.1",

      "com.chuusai" %% "shapeless" % "2.3.3",

      "com.github.andyglow" %% "scala-jsonschema" % ScalaJsonSchemaVersion,
      "com.github.andyglow" %% "scala-jsonschema-circe-json" % ScalaJsonSchemaVersion,
    )
  )

fork in run := true
