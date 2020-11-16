lazy val root = project
  .in(file("."))
  .configs(IntegrationTest.extend(Test))
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .settings(
    name := "scynamo",
    organization := "io.moia",
    crossScalaVersions := List("2.13.3", "2.12.12"),
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 12)) => scalacOptions_2_12
        case Some((2, 13)) => scalacOptions_2_13
        case _             => Seq()
      }
    },
    licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")),
    scmInfo := Some(ScmInfo(url("https://github.com/moia-dev/scynamo"), "scm:git@github.com:moia-dev/scynamo.git")),
    homepage := Some(url("https://github.com/moia-dev/scynamo")),
    libraryDependencies ++= Seq(
      "org.scalatest"          %% "scalatest"               % "3.2.3"  % Test,
      "com.chuusai"            %% "shapeless"               % "2.3.3",
      "software.amazon.awssdk"  % "dynamodb"                % "2.15.28",
      "org.typelevel"          %% "cats-core"               % "2.2.0",
      "org.typelevel"          %% "cats-testkit-scalatest"  % "2.0.0"  % Test,
      "org.scalacheck"         %% "scalacheck"              % "1.15.1" % Test,
      "org.scala-lang.modules" %% "scala-collection-compat" % "2.2.0",
      "org.scala-lang.modules" %% "scala-java8-compat"      % "0.9.1"
    )
  )
  .settings(sbtGitSettings)
  .settings(scalaFmtSettings)
  .settings(sonatypeSettings)

lazy val docs = project
  .in(file("scynamo-docs"))
  .dependsOn(root)
  .enablePlugins(MdocPlugin)
  .settings(
    Seq(
      mdocVariables := Map("VERSION" -> version.value),
      publish := {},
      publishLocal := {},
      publishArtifact := false
    )
  )

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
  "-Xlint:-byname-implicit"
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

lazy val sonatypeSettings = {
  import xerial.sbt.Sonatype._
  Seq(
    publishTo := sonatypePublishTo.value,
    sonatypeProfileName := organization.value,
    publishMavenStyle := true,
    sonatypeProjectHosting := Some(GitHubHosting("moia-dev", "scynamo", "oss-support@moia.io")),
    credentials += Credentials(Path.userHome / ".sbt" / "sonatype_credential")
  )
}
