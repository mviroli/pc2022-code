package lab.lib.movement

import it.unibo.scafi.incarnations.Incarnation
import it.unibo.scafi.space.Point3D

trait MovementLibrary extends BasicMovement_Lib with Flock_Lib {
  self : Incarnation =>

  override type P = Point3D
  type Velocity = P

  object Velocity {
    val Zero = new Velocity(0,0,0)
    def apply(x : Double, y : Double) : Velocity = new Velocity(x, y, 0)
  }

  trait MovementSupport {
    def move(velocity: Velocity) : Velocity
  }

  trait MovementProgram extends AggregateProgram with StandardSensors {
    self : MovementSupport =>
    override final def main() : Velocity = move(movementLogic())
    def movementLogic() : Velocity
  }
}

object MovementLibrary {
  type Subcomponent = Incarnation with MovementLibrary
}
