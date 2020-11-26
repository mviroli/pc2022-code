package lab.demo

import it.unibo.scafi.incarnations.BasicAbstractIncarnation
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.bridge.SimulationInfo
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.configuration.ScafiProgramBuilder
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.world.ScafiWorldInitializer.Random
import it.unibo.scafi.simulation.s2.frontend.view.{ViewSetting, WindowConfiguration}
import lab.gui.patch.RadiusLikeSimulation

object Incarnation extends BasicAbstractIncarnation
import lab.demo.Incarnation._ //import all stuff from an incarnation

object Simulation extends App {
  val programClass = classOf[Main16]
  val nodes = 50
  val neighbourRange = 200
  val (width, height) = (1024, 768)
  ViewSetting.windowConfiguration = WindowConfiguration(width, height)
  ScafiProgramBuilder (
    Random(nodes, width, height),
    SimulationInfo(programClass),
    RadiusLikeSimulation(neighbourRange),
    neighbourRender = true,
  ).launch()
}

abstract class AggregateProgramSkeleton extends AggregateProgram with StandardSensors {
  def sense1 = sense[Boolean]("sens1")
  def sense2 = sense[Boolean]("sens2")
  def sense3 = sense[Boolean]("sens3")
  def boolToInt(b: Boolean) = mux(b){1}{0}
}

class Main extends AggregateProgramSkeleton {
  def inc(x:Int):Int = x+1
  override def main() = rep(init = 0)(fun = inc)
}

class Main1 extends AggregateProgramSkeleton {
  override def main() = 1
}

class Main2 extends AggregateProgramSkeleton {
  override def main() = 2+3
}

class Main3 extends AggregateProgramSkeleton {
  override def main() = (10,20)
}

class Main4 extends AggregateProgramSkeleton {
  override def main() = Math.random()
}

class Main5 extends AggregateProgramSkeleton {
  override def main() = sense1
}

class Main6 extends AggregateProgramSkeleton {
  override def main() = if (sense1) 10 else 20
}

class Main7 extends AggregateProgramSkeleton {
  override def main() = mid()
}

class Main8 extends AggregateProgramSkeleton {
  override def main() = minHoodPlus(nbrRange)
}

class Main9 extends AggregateProgramSkeleton {
  override def main() = rep(0){_+1}
}

class Main10 extends AggregateProgramSkeleton {
  override def main() = rep(Math.random()){x=>x}
}

class Main11 extends AggregateProgramSkeleton {
  override def main() = rep[Double](0.0){x => x + rep(Math.random()){y=>y}}
}

class Main12 extends AggregateProgramSkeleton {
  import Builtins.Bounded.of_i

  override def main() = maxHoodPlus(boolToInt(nbr{sense1}))
}

class Main13 extends AggregateProgramSkeleton {
  override def main() = foldhoodPlus(0)(_+_){nbr{1}}
}

class Main14 extends AggregateProgramSkeleton {
  import Builtins.Bounded.of_i

  override def main() = rep(0){ x => boolToInt(sense1) max maxHoodPlus( nbr{x}) }
}

class Main15 extends AggregateProgramSkeleton {
  override def main() = rep(Double.MaxValue){ d => mux[Double](sense1){0.0}{minHoodPlus(nbr{d}+1.0)} }
}

class Main16 extends AggregateProgramSkeleton {
  override def main() = rep(Double.MaxValue){ d => mux[Double](sense1){0.0}{minHoodPlus(nbr{d}+nbrRange)} }
}