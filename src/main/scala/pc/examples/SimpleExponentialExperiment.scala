package pc.examples

import pc.modelling.{CTMC, CTMCExperiment}
import pc.modelling.CTMC.*
import pc.utils.Time
import scala.math.BigDecimal.double2bigDecimal
import java.util.Random

object SimpleExponentialExperiment extends App with de.sciss.chart.module.Charting:

  enum State:
    case IDLE, DONE

  export State.*
  export pc.modelling.CTMCExperiment.*
  export pc.modelling.CTMCSimulation.*

  def simpleAutomaton: CTMC[State] = CTMC.ofTransitions(
    Transition(IDLE, 1.0 --> DONE),
    Transition(DONE, 1.0 --> DONE)
  )

  val data =
    for
      t <- 0.1 to 10.0 by 0.1
      p = simpleAutomaton.experiment(
        runs = 19000,
        prop = simpleAutomaton.eventually(_ == DONE),
        rnd = new Random(),
        s0 = IDLE,
        timeBound = t.toDouble)
    yield (t, p)

  Time.timed{ println(data.mkString("\n")) }

  given ChartTheme = ChartTheme.Default
  val chart = de.sciss.chart.api.XYLineChart(data)
  chart.show("P")