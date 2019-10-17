package pc.examples

import pc.modelling.CTMC

object TryCTMC extends App {
  object State extends Enumeration {
    val idle,send,done,fail = Value
  }
  import State._
  type State = State.Value

  val channel: CTMC[State] = CTMC.ofTransitions(
    (idle,1.0,send),
    (send,100000.0,send),
    (send,200000.0,done),
    (send,100000.0,fail),
    (fail,100000.0,idle),
    (done,1.0,done)
  )
  State.values.foreach(s => println(s,channel.transitions(s)))
}
