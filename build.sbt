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

