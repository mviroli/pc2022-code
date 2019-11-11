package pc.rl

import pc.utils.Stochastics

import scala.annotation.tailrec
import pc.utils.Stochastics._

trait QRL[S,A] {

  type R = Double
  type P = Double

  // An Environment where one wants to learn
  trait Environment extends ((S, A) => (R, S)) {
    def take(s: S, a: A): (R, S)

    override def apply(s: S, a: A) = take(s, a)
  }

  // A strategy to act
  trait Policy extends (S=>A) {
    def choice(s:S): A

    override def apply(s: S) = choice(s)
  }

  // A system configuration, where "runs" can occur
  trait System {
    val environment: Environment
    val initial: S
    val terminal: S => Boolean

    def run(p: Policy): Stream[(A,S)]
  }

  // an updatable, table-oriented QFunction, to optimise selection over certain actions
  trait Q extends ((S,A)=>R) with Policy {
    def actions: Set[A]
    def update(s:S, a:A, v: R): Q

    def value(s:S): R = actions.map{this(s,_)}.max
    def choice(s:S): A = actions.maxBy{this(s,_)}
    def vFunction(): V = value _
  }

  // A VFunction
  trait V extends (S=>R)

  // The Qlearning system, with parameters
  trait Learning {
    val system: System
    val gamma: Double
    val alpha: Double
    val q0: Q

    def updateQ(s: S, qf: Q): (S, Q)
    def runEpisodes(episodes: Int, qf: Q): Q
  }

  // for visualization purposes, also show Function
  def learnAndShow(l: Learning, episodes: Int): (V,Policy)

  // the top-level learning call, yielding the policy
  def learn(l: Learning, episodes: Int): Policy = learnAndShow(l,episodes)._2
}