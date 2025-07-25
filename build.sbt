lazy val root = project
  .in(file("."))
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .settings(
    name               := "scynamo",
    organization       := "io.moia",
    crossScalaVersions := List("2.13.16", "2.12.20"),
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 12)) => scalacOptions_2_12
        case Some((2, 13)) => scalacOptions_2_13
        case _             => Seq()
      }
    },
    licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")),
    scmInfo       := Some(ScmInfo(url("https://github.com/moia-oss/scynamo"), "scm:git@github.com:moia-oss/scynamo.git")),
    homepage      := Some(url("https://github.com/moia-oss/scynamo")),
    versionScheme := Some("early-semver"),
    libraryDependencies ++= Seq(
      "org.scalatest"          %% "scalatest"               % "3.2.19" % Test,
      "com.chuusai"            %% "shapeless"               % "2.3.13",
      "software.amazon.awssdk"  % "dynamodb"                % "2.32.4",
      "org.typelevel"          %% "cats-core"               % "2.13.0",
      "org.typelevel"          %% "cats-testkit-scalatest"  % "2.1.5"  % Test,
      "org.scalacheck"         %% "scalacheck"              % "1.18.1" % Test,
      "org.scala-lang.modules" %% "scala-collection-compat" % "2.13.0"
    )
  )
  .settings(sbtGitSettings)
  .settings(scalaFmtSettings)
  .settings(sonatypeSettings)
  .settings(mimaSettings)

lazy val docs = project
  .in(file("scynamo-docs"))
  .dependsOn(root)
  .enablePlugins(MdocPlugin)
  .settings(
    mdocVariables                                   := Map("VERSION" -> version.value),
    publish                                         := {},
    publishLocal                                    := {},
    publishArtifact                                 := false,
    libraryDependencies += "org.scala-lang.modules" %% "scala-java8-compat" % "1.0.2"
  )

lazy val scalacOptions_2_12 = Seq(
  "-unchecked",
  "-deprecation",
  "-language:_",
  "-release",
  "8",
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
  "-release",
  "8",
  "-encoding",
  "UTF-8",
  "-Xfatal-warnings",
  "-Ywarn-dead-code",
  "-Ymacro-annotations",
  "-Xlint:_,-byname-implicit,-unused",
  "-Wunused:_,-imports,-synthetics",
  "-Xsource:3"
)

lazy val sbtVersionRegex = "v(\\d+.\\d+.\\d+)-?(.*)?".r

lazy val sbtGitSettings = Seq(
  git.useGitDescribe       := true,
  git.baseVersion          := "0.0.0",
  git.uncommittedSignifier := None,
  git.gitTagToVersionNumber := {
    case sbtVersionRegex(v, "")         => Some(v)
    case sbtVersionRegex(v, "SNAPSHOT") => Some(s"$v-SNAPSHOT")
    case sbtVersionRegex(v, s)          => Some(s"$v-$s-SNAPSHOT")
    case _                              => None
  }
)

lazy val scalaFmtSettings = Seq(
  scalafmtOnCompile := true
)

lazy val sonatypeSettings = {
  import xerial.sbt.Sonatype._
  Seq(
    publishTo              := sonatypePublishTo.value,
    sonatypeProfileName    := organization.value,
    publishMavenStyle      := true,
    sonatypeProjectHosting := Some(GitHubHosting("moia-oss", "scynamo", "oss-support@moia.io")),
    credentials += Credentials(Path.userHome / ".sbt" / "sonatype_credential")
  )
}

lazy val mimaSettings = Seq(
  mimaPreviousArtifacts := Set.empty
)
