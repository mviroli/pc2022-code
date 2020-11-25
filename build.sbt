lazy val scalaProjectVersion = "2.12.4"

lazy val commonSetting = Seq(
  scalaVersion := "2.12.4",
  version := "0.1"
)

// build.sbt
val scafi_version = "v0.3.3"

val scafi_core  =  "it.unibo.apice.scafiteam" %% "scafi-core" % scafi_version
val scafi_simulator  =  "it.unibo.apice.scafiteam" %% "scafi-simulator" % scafi_version
val scafi_simulator_gui =  "it.unibo.apice.scafiteam" %% "scafi-simulator-gui-new" % scafi_version
val scalafx = "org.scalafx" %% "scalafx" % "8.0.144-R12"

libraryDependencies ++= Seq(scafi_core, scafi_simulator, scafi_simulator_gui, scalafx)

// Determine OS version of JavaFX binaries
lazy val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux") => "linux"
  case n if n.startsWith("Mac") => "mac"
  case n if n.startsWith("Windows") => "win"
  case _ => throw new Exception("Unknown platform!")
}
libraryDependencies ++= Seq(scalafx)
libraryDependencies ++= javaFXModules.map( m=>
  "org.openjfx" % s"javafx-$m" % "14.0.1" % "runtime" classifier osName
)
// Add JavaFX dependencies
lazy val javaFXModules = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")

