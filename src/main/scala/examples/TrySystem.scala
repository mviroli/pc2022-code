package examples

import pc.modelling.System

object TrySystem extends App {
  // Specification of my data-type for states
  object state extends Enumeration {
    val idle,send,done,fail = Value
  }
  type State = state.Value
  import state._

  // System specification
  val channel: System[State] = System.ofTransitions(
    idle->send,
    send->send,
    send->done,
    send->fail,
    fail->idle //,done->done
  )

  // Analysis, by querying
  println(channel.normalForm(idle))
  println(channel.normalForm(done))
  println(channel.next(idle))
  println(channel.next(send))
  println("P1  "+channel.paths(idle,1).toList)
  println("P2  "+channel.paths(idle,2).toList)
  println("P3  "+channel.paths(idle,3).toList)
  println("P4  "+channel.paths(idle,4).toList)
  println("CMP:\n"+channel.completePaths(idle).take(100).toList.mkString("\n"))
}
