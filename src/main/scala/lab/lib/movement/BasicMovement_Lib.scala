package lab.lib.movement

import it.unibo.scafi.space.{Point2D, Point3D}

trait BasicMovement_Lib {
  self : MovementLibrary.Subcomponent =>

  trait Movement2D {
    self : FieldCalculusSyntax with StandardSensors =>

    def clockwiseRotation(center : P) : Velocity = {
      val centerVector = currentPosition() - center
      Point2D(centerVector.y, - centerVector.x).normalized
    }

    def anticlockwiseRotation(center : P) : Velocity = -clockwiseRotation(center)

    def goToPoint(point : P) : Velocity = (point - currentPosition()).normalized

    def standStill : Velocity = Velocity.Zero
  }

  trait Steering {
    self : Movement2D with FieldCalculusSyntax with StandardSensors =>

    def explore(minCoord : P, maxCoord : P, trajectoryTime : Int, reachGoalRange : Double = 0) : Velocity = {
      require(trajectoryTime > 0)
      def randomCoord : Point2D = Point2D(
        minCoord.x + (math.random() * (maxCoord.x - minCoord.y)), //todo use correct random
        minCoord.y + (math.random() * (maxCoord.y - minCoord.y)))
      val (_, _, velocity) = rep((randomCoord, trajectoryTime, Point3D.Zero)){
        case (goal, decay, v) if (decay == 0) => (randomCoord, trajectoryTime, v)
        case (goal, decay, v) if (goal.distance(currentPosition()) < reachGoalRange)=> (goal, 0, goToPoint(goal))
        case (goal, decay, v) => (goal, decay - 1, goToPoint(goal))
      }
      velocity
    }
  }
}
