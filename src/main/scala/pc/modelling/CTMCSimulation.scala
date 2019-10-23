package pc.modelling

import java.util.Random

import pc.utils.Stochastics

trait CTMCSimulation[S] { self: CTMC[S] =>

  type Trace[A] = Stream[(Double,A)]

  def newSimulationTrace(s0: S, rnd: Random): Trace[S] =
    Stream.iterate( (0.0,s0) ){ case (t,s) => {
      if (transitions(s).isEmpty) (t,s) else {
        val next = Stochastics.cumulative(transitions(s).toList)
        val sumR = next.last._1
        val choice = Stochastics.draw(next)(rnd)
        (t + Math.log(1 / rnd.nextDouble()) / sumR, choice)
      }
    }}
}

object CTMCSimulation {
  def apply[S](ctmc: CTMC[S]): CTMCSimulation[S] =
    new CTMC[S] with CTMCSimulation[S]{
      override def transitions(s: S) = ctmc.transitions(s)
    }
}

