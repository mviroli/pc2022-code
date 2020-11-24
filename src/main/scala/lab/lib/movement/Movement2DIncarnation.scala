package lab.lib.movement

import it.unibo.scafi.incarnations.BasicAbstractIncarnation

object Movement2DIncarnation extends MovementLibrary with BasicAbstractIncarnation {

  trait Scafi2DSupport extends MovementSupport {
    override def move(velocity: Velocity): Velocity = velocity
  }

  trait Movement2DProgram extends MovementProgram with Scafi2DSupport {}

}
