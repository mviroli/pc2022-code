package pc.modelling

import java.util.Random

trait CTMCAnalysis[S] extends CTMCSimulation[S] { self: CTMC[S] =>

  type Property[A] = Trace[A] => Boolean

  // globally is simply achieved by equivalence not G x= F not x
  def eventually[A](filt: A=>Boolean): Property[A] =
    (trace) => trace.map{case (t,a) => filt(a)}.exists(x=>x)

  // takes s property and makes it time bounded by the magics of streams
  def bounded[A](timeBound: Double)(prop: Property[A]): Property[A] =
    trace => prop(trace.takeWhile { case (t,a) => t<=timeBound })

  // s PRISM-like experiment, giving s statistical result (in [0,1])
  def experiment(runs: Int = 10000, prop: Property[S], rnd:Random = new Random, s0:S, timeBound: Double): Double =
    (0 to runs).count(i=>bounded(timeBound)(prop)(newSimulationTrace(s0,rnd))).toDouble/runs
}

object CTMCAnalysis {
  def apply[S](ctmc: CTMC[S]): CTMCAnalysis[S] = new CTMC[S] with CTMCAnalysis[S]{
    override def transitions(s: S) = ctmc.transitions(s)
  }
}