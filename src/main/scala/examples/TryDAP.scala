package examples

import java.util.Random

import pc.modelling.{CTMCAnalysis, DAP, DAPHelpers, MSet}

object tryDAP extends App {
  object place extends Enumeration {
    val a,b,c = Value
  }
  type Place = place.Value
  type ID = (Int,Int)
  import place._
  import DAP._

  val gossip = DAP[Place](
    Rule(MSet(a,a),m=>1000,MSet(a),MSet()),   // a|a --1000--> a
    Rule(MSet(a),m=>1,MSet(a),MSet(a)),       // a --1--> a|^a
  )
  val system = DAP.toCTMC[ID,Place](gossip)
  val net = DAPHelpers.createRectangularGrid(5,5)
  // an `a` initial on top left
  val state = State[ID,Place](MSet(Token((0,0),a)),MSet(),net)

  val analysis = CTMCAnalysis(system)
  analysis.newSimulationTrace(state,new Random).take(50).toList.foreach(
    step => {
      println(step._1)
      println(DAPHelpers.simpleGridStateToString[Place](step._2,a))
    })
}