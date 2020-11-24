package lab.simulation

import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.bridge.SimulationInfo
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.configuration.ScafiProgramBuilder
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.world.ScafiWorldInitializer.Grid
import it.unibo.scafi.simulation.s2.frontend.view.ViewSetting
import lab.demo._
import lab.gui.patch.RadiusLikeSimulation

object GraphicalSimulation extends App {
  val distance = 40
  val rows = 10
  val cols = 10

  ViewSetting.labelFontSize = 5
  ScafiProgramBuilder (
    Grid(distance, rows, cols),
    SimulationInfo(program = classOf[Program1]),
    RadiusLikeSimulation(distance),
    neighbourRender = true
  ).launch()

}
