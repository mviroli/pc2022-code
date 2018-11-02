package pc.modelling

import java.util.Random

trait CTMCAnalysis[A] { self: CTMC[A] =>

  type Trace[A] = Stream[(Double,A)]

  def newSimulationTrace(a0: A, rnd: Random): Trace[A] =
    Stream.iterate( (0.0,a0) ){ case (t,a) => {
      val next = nextWithRate(a).toList.scanLeft((0.0, a0)) { case ((r,a),(r2,a2)) => (r+r2,a2) }
      if (next.size == 1) (t,a) else {
        val rnd1 = rnd.nextDouble() * next.last._1
        val choice = next.collectFirst{ case (p, a) if p >= rnd1 => a }.get
        (t + Math.log(1 / rnd.nextDouble()) / next.last._1, choice)
      }
    }}

  type Property[A] = Trace[A] => Boolean

  // globally is simply achieved by equivalence not G x= F not x
  def eventually[A](filt: A=>Boolean): Property[A] =
    (trace) => trace.map{case (t,a) => filt(a)}.exists(x=>x)

  // takes a property and makes it time bounded by the magics of streams
  def bounded[A](timeBound: Double)(prop: Property[A]): Property[A] =
    trace => prop(trace.takeWhile { case (t,a) => t<=timeBound })

  // a PRISM-like experiment, giving a statistical result (in [0,1])
  def experiment(runs: Int = 10000, prop: Property[A], rnd:Random = new Random, a0:A, timeBound: Double): Double =
    (0 to runs).count(i=>bounded(timeBound)(prop)(newSimulationTrace(a0,rnd))).toDouble/runs
}

object CTMCAnalysis {
  def apply[A](ctmc: CTMC[A]): CTMCAnalysis[A] = new CTMC[A] with CTMCAnalysis[A]{
    override def nextWithRate(a: A) = ctmc.nextWithRate(a)
  }

  // facility to track time, just embed the computation in the input
  def timed[A](v: =>A):A = {
    val t0 = java.lang.System.nanoTime
    try{ v } finally println("Timed op (msec): "+(java.lang.System.nanoTime-t0)/1000000)
  }
}