package lab.gui.patch

import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.bridge.ScafiWorldIncarnation._
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.bridge.scafiSimulationExecutor.world
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.bridge.{ScafiBridge, ScafiSimulationInitializer, SimulationInfo}
import it.unibo.scafi.simulation.s2.frontend.model.sensor.SensorConcept

case class RadiusLikeSimulation(radius : Double = 0.0) extends ScafiSimulationInitializer {
  import AsyncExecutor.world._
  override def create(scafiSimulationSeed : SimulationInfo): ScafiBridge = {
    val bridge = AsyncExecutor
    val proto = () => {
      val w = bridge.world
      val nodes: Map[ID, P] = w.nodes.map{n => n.id -> new P(n.position.x,n.position.y,n.position.z)}.toMap
      val createdSpace  = new Basic3DSpace(nodes,radius)
      val createdDevs =  nodes.map { case (d, p) => d -> new DevInfo(d, p,
        nsns = nsns => nbr => null)
      }
      val res : SpaceAwareSimulator = new SpaceAwareSimulator(simulationSeed = System.nanoTime(),randomSensorSeed = System.nanoTime(),
        space = createdSpace,
        devs = createdDevs)
      w.nodes  foreach { x =>
        x.devices filter {device => device.stream == SensorConcept.sensorInput} foreach { y => res.chgSensorValue(y.name,Set(x.id),y.value)}
      }
      res.getAllNeighbours().foreach { x => world.network.setNeighbours(x._1,x._2.toSet)}
      res
    }
    bridge.simulationPrototype = Some(proto)
    bridge.simulationInfo = scafiSimulationSeed
    bridge
  }
}