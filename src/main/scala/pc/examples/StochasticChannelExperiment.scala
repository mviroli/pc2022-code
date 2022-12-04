package pc.examples
import scala.math.BigDecimal.double2bigDecimal
import pc.utils.Time

import java.util.Random


object StochasticChannelExperiment extends App with de.sciss.chart.module.Charting:
  import pc.modelling.CTMCExperiment.*
  import pc.modelling.CTMCSimulation.*
  import pc.examples.StochasticChannel.*

  val data =
    for
      t <- 0.1 to 10.0 by 0.1
      p = stocChannel.experiment(
        runs = 26000,
        prop = stocChannel.eventually(_ == DONE),
        rnd = new Random(),
        s0 = IDLE,
      timeBound = t.toDouble)
    yield (t, p)

  Time.timed{ println(data.mkString("\n")) }

  given ChartTheme = ChartTheme.Default
  val chart = de.sciss.chart.api.XYLineChart(data)
  chart.show("P")