name := "scafi-experiment"

version := "0.1"

scalaVersion := "2.12.4"
// build.sbt
val scafi_version = "v0.3.3"
resolvers += Resolver.mavenCentral
resolvers += Resolver.sonatypeRepo("releases")
val scafi_core  =  "it.unibo.apice.scafiteam" %% "scafi-core" % scafi_version
val scafi_simulator  =  "it.unibo.apice.scafiteam" %% "scafi-simulator" % scafi_version
val scopt = "com.github.scopt" %% "scopt" % "4.0.0-RC2"
val scafi_simulator_gui =  "it.unibo.apice.scafiteam" %% "scafi-simulator-gui-new" % scafi_version
val scalafx = "org.scalafx" %% "scalafx" % "8.0.144-R12"
libraryDependencies ++= Seq(scafi_core, scafi_simulator, scafi_simulator_gui, scalafx)