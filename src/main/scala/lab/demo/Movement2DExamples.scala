package lab.demo

import lab.lib.movement.Movement2DIncarnation._
import lab.lib.movement._

class ConstantMovement extends Movement2DProgram {
  override def movementLogic(): Velocity = (1.0, 1.0)
}

class BasicMovement extends Movement2DProgram with Movement2D {
  override def movementLogic(): Velocity = clockwiseRotation((0.0, 0.0))
}