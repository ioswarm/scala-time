lazy val settings = Seq(
	name := "scala-time"
	, organization := "de.ioswarm"
	, version := "0.1.1"
	, scalaVersion := "2.12.8"
	, scalacOptions ++= Seq(
		"-language:_"
		, "-unchecked"
		, "-deprecation"
		, "-encoding", "UTF-8"
	)
)

lazy val scala_time = project.in(file("."))
	.settings(settings)
	.aggregate(
		time
		, argonaut
	)

lazy val time = project.in(file("time"))
	.settings(settings)
  .settings(
		libraryDependencies ++= Seq(
			lib.config
			, lib.runtimeLib
			, lib.scalaTest
		)
	)

lazy val argonaut = project.in(file("json/argonaut"))
  .settings(settings)
  .settings(
		name := "scala-time-argonaut"
		, libraryDependencies ++= Seq(
			lib.argonaut

			, lib.scalaTest
		)
	)
  .dependsOn(
		time
	)

lazy val lib = new {
  object Version{
    val argonaut = "6.2.2"

		val scalaTest = "3.0.5"
		val config = "1.3.3"
  }

	val config = "com.typesafe" % "config" % Version.config
  val argonaut = "io.argonaut" %% "argonaut" % Version.argonaut

	val runtimeLib = "com.thesamet.scalapb" %% "scalapb-runtime" % "0.8.2"

	val scalaTest = "org.scalatest" %% "scalatest" % Version.scalaTest % Test

}