lazy val settings = Seq(
	name := "scala-time"
	, organization := "de.ioswarm"
	, version := "0.1.0"
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
	)

lazy val time = project.in(file("time"))
	.settings(settings)
  .settings(
		libraryDependencies ++= Seq(
			lib.scalaTest
		)
	)

lazy val lib = new {
  object Version{
    val argonaut = "6.2.2"

		val scalaTest = "3.0.5"
  }

  val argonaut = "io.argonaut" %% "argonaut" % Version.argonaut

	val scalaTest = "org.scalatest" %% "scalatest" % Version.scalaTest % Test

}