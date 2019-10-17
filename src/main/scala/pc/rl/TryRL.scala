package pc.rl

import TwoWaysMDP._
import TwoWaysMDP.State
import TwoWaysMDP.Action._


object TryRL extends App {
  val rl = rlTW()
  var qf = qfTW()
  qf = rl.runEpisodes(20,qf)
  (-5 to 10).map(i => (i,qf.actions.maxBy{qf(State(i),_)},qf.actions.map{qf(State(i),_)}.max))
    .foreach(println(_))
}
