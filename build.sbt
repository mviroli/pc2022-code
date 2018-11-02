name := "formalsystems"

version := "0.1"

scalaVersion := "2.12.3"

val scalatest = "org.scalatest" % "scalatest_2.12" % "3.0.1" % "test"
val scalacheck = "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
val scalachart = "com.github.wookietreiber" %% "scala-chart" % "latest.integration"

lazy val root = (project in file (".")).
  settings(Seq(scalaVersion := "2.12.3")).
  settings(name := "hello-scalatest", libraryDependencies ++= Seq (scalatest, scalacheck, scalachart))
