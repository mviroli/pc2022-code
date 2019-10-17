package pc.modelling

import java.util.Random

trait CTMCSimulation[S] { self: CTMC[S] =>

  type Trace[A] = Stream[(Double,A)]

  def newSimulationTrace(s0: S, rnd: Random): Trace[S] =
    Stream.iterate( (0.0,s0) ){ case (t,s) => {
      // TODO: Use Stochastics!
      val next = transitions(s).toList
                                .scanLeft((0.0, s0)) {case ((r,s),(r2,s2)) => (r+r2,s2)}
      if (next.size == 1) (t,s) else {
        val rnd1 = rnd.nextDouble() * next.last._1
        val choice = next.collectFirst{ case (p, s) if p >= rnd1 => s }.get
        (t + Math.log(1 / rnd.nextDouble()) / next.last._1, choice)
      }
    }}
}

object CTMCSimulation {
  def apply[S](ctmc: CTMC[S]): CTMCSimulation[S] = new CTMC[S] with CTMCSimulation[S]{
    override def transitions(s: S) = ctmc.transitions(s)
  }

  // facility to track time, just embed the computation in the input
  def timed[A](v: =>A):A = {
    val t0 = java.lang.System.nanoTime
    try{ v } finally println("Timed op (msec): "+(java.lang.System.nanoTime-t0)/1000000)
  }
}

