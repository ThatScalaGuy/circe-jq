lazy val V = new {
  val circe = "0.14.15"
  val munit = "1.0.4"
  val catsParse = "1.0.0"
}

ThisBuild / tlBaseVersion := "0.1" // your current series x.y

ThisBuild / organization := "de.thatscalaguy"
ThisBuild / organizationName := "ThatScalaGuy"
ThisBuild / startYear := Some(2022)
ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / developers := List(
  // your GitHub handle and name
  tlGitHubDev("ThatScalaGuy", "Sven Herrmann")
)

// publish website from this branch
ThisBuild / tlSitePublishBranch := Some("main")

val Scala213 = "2.13.18"
val Scala3 = "3.3.4"
ThisBuild / crossScalaVersions := Seq(Scala213, Scala3)
ThisBuild / scalaVersion := Scala213 // the default Scala

lazy val root = tlCrossRootProject.aggregate(core)

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(
    name := "circe-jq",
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core" % V.circe,
      "org.typelevel" %%% "cats-parse" % V.catsParse,
      "org.scalameta" %%% "munit" % V.munit % Test
    )
  )

lazy val docs = project.in(file("site")).enablePlugins(TypelevelSitePlugin)
