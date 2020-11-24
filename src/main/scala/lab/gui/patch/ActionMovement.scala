package lab.gui.patch

import it.unibo.scafi.simulation.MetaActionManager
import it.unibo.scafi.simulation.MetaActionManager.MetaAction
import it.unibo.scafi.simulation.s2.frontend.incarnation.scafi.bridge.{MetaActionProducer, SimulationExecutor, scafiSimulationExecutor}
import it.unibo.scafi.space.Point3D

class ActionMovement extends MetaActionProducer[(Double, Double)] {

  private var action : (Any) => (Option[(Double,Double)]) = {
    case p : Point3D => Some((p.x, p.y))
    case _ => None
  }
  override def valueParser_=(action: Any => Option[(Double, Double)]): Unit = this.action = action
  override def valueParser: Any => Option[(Double, Double)] = action

  override def apply(id: Int, dt: (Double, Double)): MetaAction = if (dt != (0.0, 0.0)) {
    SimulationExecutor.Instance.get.contract.simulation.get.NodeDtMovement(id, dt)
  } else {
    MetaActionManager.EmptyAction
  }

  override def toString: String = "dt-meta-action"

}
