package pc.rl.model

import pc.utils.Stochastics
import pc.utils.Stochastics.{cumulative, draw}

import scala.util.Random

trait QRL[S, A]:
  given Random = new scala.util.Random()

  type R = Double // Reward
  type P = Double // Probability

  type Environment = (S, A) => (R, S)

  class MDP(transitions: S => Set[(A, P, R, S)]) extends Environment:
    override def apply(s: S, a: A): (R, S) =
      draw(cumulative(transitions(s).
        collect { case (`a`, p, r, s) => (p, (r, s)) }.toList))

  object MDP:

    def ofFunction(f: PartialFunction[S, Set[(A, P, R, S)]]): MDP =
      new MDP(s => f.applyOrElse(s, (x: S) => Set()))

    def ofRelation(rel: Set[(S, A, P, R, S)]): MDP = ofFunction(
      s => rel filter (_._1 == s) map (t => (t._2, t._3, t._4, t._5) )
    )

    def ofTransitions(rel: (S, A, P, R, S)*): MDP = ofRelation(rel.toSet)

  // A strategy to act
  type Policy = S => A

  // A system configuration, where "runs" can occur
  trait System:
    val environment: Environment
    val initial: S
    val terminal: S => Boolean

    def run(p: Policy): LazyList[(A, S)]

  // A VFunction
  type VFunction = S => R

  // an updatable, table-oriented QFunction
  trait Q extends ((S, A) => R):
    def actions: Set[A]

    def update(s: S, a: A, v: R): Q

    def bestPolicy: Policy = s => actions.maxBy(this (s, _))

    import Stochastics.*
    def explorativePolicy(f: P): Policy = s => s match
      case s if drawFiltered(_ < f) => uniformDraw(actions)
      case s => bestPolicy(s)

    def vFunction: VFunction = s => actions.map(this (s, _)).max

  // The learning system, with parameters
  trait LearningProcess:
    val system: System
    val gamma: Double
    val alpha: Double
    val epsilon: Double
    val q0: Q

    def updateQ(s: S, qf: Q): (S, Q)

    def learn(episodes: Int, episodeLength: Int, qf: Q): Q
