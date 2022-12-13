package pc.rl.examples

import pc.rl.model.QMatrix

object TryQMatrix extends App:

  import pc.rl.model.QMatrix.Action.*
  import pc.rl.model.QMatrix.*

  val rl: QMatrix.Facade = Facade(
    width = 5,
    height = 5,
    initial = (0,0),
    terminal = {case _=>false},
    reward = { case ((1,0),_) => 10; case ((3,0),_) => 5; case _ => 0},
    jumps = { case ((1,0),_) => (1,4); case ((3,0),_) => (3,2) },
    gamma = 0.9,
    alpha = 0.5,
    epsilon = 0.3,
    v0 = 1
  )

  val q0 = rl.qFunction
  println(rl.show(q0.vFunction,"%2.1f"))
  val q1 = rl.makeLearningInstance().learn(20000,100,q0)
  println(rl.show(q1.vFunction,"%2.1f"))
  println(rl.show(s => actionToString(q1.bestPolicy(s)),"%7s"))