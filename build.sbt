lazy val root = (project in file("."))
  .configs(IntegrationTest.extend(Test))
  .enablePlugins(
    GitVersioning,
    GitBranchPrompt
  )
  .settings(
    name := "dynamo-format",
    organization := "io.moia",
    scalaVersion := "2.13.1",
    scalacOptions := scalaCompilerOptions,
    scalafmtOnCompile := true,
    resolvers += "Artifactory".at("https://moiadev.jfrog.io/moiadev/sbt-release"),
    credentials ++= Seq(Path.userHome / ".ivy2" / ".credentials").filter(_.exists).map(Credentials(_)),
    credentials ++= Seq("ARTIFACTORY_USER")
      .filter(sys.env.isDefinedAt)
      .map(user => Credentials("Artifactory Realm", "moiadev.jfrog.io", sys.env(user), sys.env("ARTIFACTORY_APIKEY"))),
    publishTo := Some("Artifactory Realm".at("https://moiadev.jfrog.io/moiadev/sbt-release-local/")),
    libraryDependencies ++= Seq(
      "org.scalatest"          %% "scalatest" % "3.1.0" % Test,
      "com.chuusai"            %% "shapeless" % "2.3.3",
      "software.amazon.awssdk" % "dynamodb"   % "2.10.65",
      "org.typelevel" %% "cats-core" % "2.1.0"
    )
  )
  .settings(sbtGitSettings)
  .settings(scalaFmtSettings)

lazy val scalaCompilerOptions = Seq(
  "-target:11",
  "-deprecation",
  "-encoding",
  "utf-8",
  "-explaintypes",
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Ywarn-dead-code",
  "-Ywarn-extra-implicit",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused:implicits",
  "-Ywarn-unused:imports",
  "-Ywarn-unused:locals",
  "-Ywarn-unused:params",
  "-Ywarn-unused:patvars",
  "-Ywarn-unused:privates",
  "-Ywarn-value-discard"
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
