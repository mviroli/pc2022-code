package pc.examples

import pc.modelling.CTMC

object StochasticChannel:
  enum State:
    case IDLE, SEND, DONE, FAIL;

  export State.*
  export pc.modelling.CTMCSimulation.*

  def stocChannel: CTMC[State] = CTMC.ofTransitions(
    (IDLE,1.0,SEND),
    (SEND,100000.0,SEND),
    (SEND,200000.0,DONE),
    (SEND,100000.0,FAIL),
    (FAIL,100000.0,IDLE),
    (DONE,1.0,DONE)
  )

@main def mainStochasticChannel() =  // example run
  import StochasticChannel.*
  State.values.foreach(s => println(s"$s,${stocChannel.transitions(s)}"))
