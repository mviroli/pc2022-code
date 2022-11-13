package pc.modelling

import java.util.Random
import pc.utils.Stochastics

object CTMCSimulation:

  type Trace[A] = LazyList[(Double,A)]

  extension [S](self: CTMC[S])
    def newSimulationTrace(s0: S, rnd: Random): Trace[S] =
      LazyList.iterate((0.0, s0)) { case (t, s) =>
        if self.transitions(s).isEmpty
        then
          (t, s)
        else
          val next = Stochastics.cumulative(self.transitions(s).toList)
          val sumR = next.last._1
          val choice = Stochastics.draw(next)(using rnd)
          (t + Math.log(1 / rnd.nextDouble()) / sumR, choice)
      }
