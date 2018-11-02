package examples

object TryCTMCExperiment extends App {

  import pc.modelling.CTMCAnalysis._
  import pc.modelling.{CTMC, CTMCAnalysis}

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
  val data = for (t <- (0.1 to 10.0 by 0.1).toStream;
                  p = channelAnalysis.experiment(
                        runs = 10000,
                        prop = channelAnalysis.eventually(_ == done),
                        a0 = idle,
                        timeBound = t)) yield (t, p)

  timed{ println(data.mkString("\n")) }
  scalax.chart.api.XYLineChart(data).show() // with dependencies on scala-chart
}