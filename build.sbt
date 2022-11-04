scalaVersion := "3.1.1"

lazy val root = (project in file("."))
  .settings(
    name := "pc2-code",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.12" % Test,
      "org.scalacheck" %% "scalacheck" % "1.15.4" % Test
      //"com.github.wookietreiber" %% "scala-chart" % "0.4.2"
    )
  )
