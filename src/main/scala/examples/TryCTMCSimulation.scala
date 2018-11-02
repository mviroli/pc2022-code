package examples

object TryCTMCSimulation extends App {

  import java.util.Random
  import pc.modelling.{CTMC, CTMCAnalysis}
  import pc.modelling.CTMCAnalysis._

  object State extends Enumeration {
    val idle,send,done,fail = Value
  }
  import State._

  val channel: CTMC[State.Value] = CTMC.ofTransitions(
    (idle,1.0,send),
    (send,100000.0,send),
    (send,200000.0,done),
    (send,100000.0,fail),
    (fail,100000.0,idle),
    (done,1.0,done)
  )

  val channelAnalysis = CTMCAnalysis(channel)
  timed{
    println(channelAnalysis.newSimulationTrace(idle, new Random).take(10).toList.mkString("\n"))
  }
}