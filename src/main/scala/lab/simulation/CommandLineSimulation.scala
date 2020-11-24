package lab.simulation
import it.unibo.scafi.config.GridSettings
import it.unibo.scafi.incarnations.{BasicAbstractSpatialSimulationIncarnation, BasicSimulationIncarnation}
import lab.demo._

import scala.util.Random

object CommandLineSimulation extends App {
  object MyIncarnation extends BasicAbstractSpatialSimulationIncarnation
  import MyIncarnation._
  val program = (new BasicMovement).asInstanceOf[AggregateInterpreter]
  /**
   * little world example. Elements are dispatch in a grid like these:
   * x - x - x
   * |   |   |
   * x - x - x
   * |   |   |
   * x - x - x
   */
  val rows = 3
  val cols = 3
  val stepX = 10
  val stepY = 10
  val simulator = MyIncarnation.simulatorFactory.gridLike(
    GridSettings(rows, cols, stepX, stepY),
    stepX
  )

  val iteration = 100
  val seed = 0
  val random = new Random(seed)
  val minNodeId = 1

  def toGridWorld() : String = {
    (1 to (rows * cols))
      .grouped(rows)
      .map(row => row.map(simulator.`export`))
      .map(row => row.map(option => option.map(_.root[Any]())))
      .map(row => row.map(_.getOrElse("_").toString))
      .map(row => row.mkString("", "\t", ";"))
      .mkString("\n")
  }
  for (i <- 0 to iteration) {
    println(s"iteration $i")
    val device = random.nextInt(cols * rows) + minNodeId
    println(s"execution fired on $device")
    simulator.exec(program, program.main(), device)
    println("---- NEW WORLD ---")
    println(toGridWorld())
  }
}
