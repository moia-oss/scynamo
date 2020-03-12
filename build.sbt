lazy val root = (project in file("."))
  .configs(IntegrationTest.extend(Test))
  .enablePlugins(
    GitVersioning,
    GitBranchPrompt
  )
  .settings(
    name := "scynamo",
    organization := "io.moia",
    scalaVersion := "2.13.1",
    crossScalaVersions := List("2.13.1", "2.12.10"),
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 12)) => scalacOptions_2_12
        case Some((2, 13)) => scalacOptions_2_13
        case _             => Seq()
      }
    },
    scalafmtOnCompile := true,
    credentials ++= Seq(Path.userHome / ".ivy2" / ".credentials").filter(_.exists).map(Credentials(_)),
    credentials ++= Seq("ARTIFACTORY_USER")
      .filter(sys.env.isDefinedAt)
      .map(user => Credentials("Artifactory Realm", "moiadev.jfrog.io", sys.env(user), sys.env("ARTIFACTORY_APIKEY"))),
    publishTo := Some("Artifactory Realm".at("https://moiadev.jfrog.io/moiadev/sbt-release-local/")),
    libraryDependencies ++= Seq(
      "org.scalatest"          %% "scalatest"               % "3.1.1" % Test,
      "com.chuusai"            %% "shapeless"               % "2.3.3",
      "software.amazon.awssdk" % "dynamodb"                 % "2.10.65",
      "org.typelevel"          %% "cats-core"               % "2.1.0",
      "org.scalacheck"         %% "scalacheck"              % "1.14.3" % Test,
      "org.scala-lang.modules" %% "scala-collection-compat" % "2.1.4"
    )
  )
  .settings(sbtGitSettings)
  .settings(scalaFmtSettings)

lazy val scalacOptions_2_12 = Seq(
  "-unchecked",
  "-deprecation",
  "-language:_",
  "-target:jvm-1.8",
  "-encoding",
  "UTF-8",
  "-Xfatal-warnings",
  "-Ywarn-unused-import",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-inaccessible",
  "-Ywarn-infer-any",
  "-Ywarn-nullary-override",
  "-Ywarn-nullary-unit",
  "-Ypartial-unification",
  "-Xlint"
)

lazy val scalacOptions_2_13 = Seq(
  "-unchecked",
  "-deprecation",
  "-language:_",
  "-target:jvm-1.8",
  "-encoding",
  "UTF-8",
  "-Xfatal-warnings",
  "-Ywarn-dead-code",
  "-Ymacro-annotations",
  "-Xlint"
)

lazy val sbtVersionRegex = "v([0-9]+.[0-9]+.[0-9]+)-?(.*)?".r

lazy val sbtGitSettings = Seq(
  git.useGitDescribe := true,
  git.baseVersion := "0.0.0",
  git.uncommittedSignifier := None,
  git.gitTagToVersionNumber := {
    case sbtVersionRegex(v, "")         => Some(v)
    case sbtVersionRegex(v, "SNAPSHOT") => Some(s"$v-SNAPSHOT")
    case sbtVersionRegex(v, s)          => Some(s"$v-$s-SNAPSHOT")
    case _                              => None
  }
)

lazy val scalaFmtSettings =
  Seq(
    scalafmtOnCompile := true
  ) ++ inConfig(IntegrationTest.extend(Test))(org.scalafmt.sbt.ScalafmtPlugin.scalafmtConfigSettings)
