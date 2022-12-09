package pc.rl.examples

import TwoWaysMDP._

object TryRL extends App:
  val rl = TwoWaysMDP.rl.rlTW()
  var qf = TwoWaysMDP.rl.qfTW()
  qf = rl.learn(20, 10, qf)
  for
    i <- -5 to 10
    action = qf.actions.maxBy(qf(State(i), _))
    reward = qf.actions.map(qf(State(i), _)).max
  do
    println( (i,action,reward) )
