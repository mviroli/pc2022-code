package pc.rl

import pc.rl.MDP.R
import pc.utils.Stochastics

import scala.annotation.tailrec
import pc.utils.Stochastics._

// A system/environment, modelled as A (hidden) MDP, i.e., an oracle
trait MDPSystem[S,A] extends ((S,A) => (MDP.R,S)) {

  def take(s: S, a: A): (MDP.R,S)
  override def apply(s: S, a: A) = take(s,a)
}

// A state-based, true MDP, with embedded probabilistic drawing of "response"
trait MDP[S,A] extends MDPSystem[S,A] {

  def transitions(s: S): Set[(A,MDP.P,MDP.R,S)]
  def take(s: S, a: A): (MDP.R,S) = Stochastics.draw(cumulative(transitions(s).collect{ case (`a`,p,r,s) => (p,(r,s))}.toList))
}

// MDP factories
object MDP {

  type R = Double // Reward
  type P = Double // Probability

  def ofFunction[S,A](f: PartialFunction[S,Set[(A,P,R,S)]]): MDPSystem[S,A] =
    new MDP[S,A]{
      override def transitions(s: S) = f.applyOrElse(s, (x: S)=>Set())
    }

  def ofRelation[S,A](rel: Set[(S,A,P,R,S)]): MDPSystem[S,A] = ofFunction[S,A]{
    case s => rel filter {_._1 == s} map { t=>(t._2,t._3,t._4,t._5)}
  }

  def ofTransitions[S,A](rel: (S,A,P,R,S)*): MDPSystem[S,A] = ofRelation(rel.toSet)

  def ofOracle[S,A](oracle: (S,A)=>(R,S)): MDPSystem[S,A] = new MDPSystem[S,A] {
    override def take(s: S, a: A): (R, S) = oracle(s,a)
  }
}

// an updatable, table-oriented QFunction, to optimise selection over certain actions
trait QFunction[S,A] extends ((S,A)=>MDP.R) {
  def actions: Set[A]
  def update(s:S, a:A, v: MDP.R): QFunction[S,A]
  def value(s:S): MDP.R = actions.map{this(s,_)}.max
  def detTake(s:S): A = actions.maxBy{this(s,_)}
  def vFunction(): VFunction[S] = value _
}

// A VFunction
trait VFunction[S] extends (S=>MDP.R)

// QFunction factory
object QFunction {
  val TERMINAL_VALUE: MDP.R = 0.0
  // need to know the actions to select, the initial value of each action, and possibly A notion of terminal state(s)
  def apply[S,A](actionSet: Set[A], v0: MDP.R = 0.0, terminal: S=>Boolean = (s:S)=>false): QFunction[S,A] = new QFunction[S,A]{
    val map: collection.mutable.Map[(S,A),MDP.R] = collection.mutable.Map()
    override def apply(s: S, a: A) = if (terminal(s)) TERMINAL_VALUE else map.getOrElse(s->a,v0)
    override def update(s: S, a: A, v: Double): QFunction[S,A] = { map += ((s->a)->v); this }
    override def actions = actionSet
    override def toString = map.toString
  }
}

trait QRL[S,A] {
  val mdp: MDPSystem[S,A]
  val initial: S
  val terminal: S => Boolean
  def updateQ(s: S, qf: QFunction[S, A]): (S, QFunction[S, A])
  def runEpisodes(episodes: Int, qf: QFunction[S,A]): QFunction[S,A]
  def run(qf: QFunction[S,A]): Stream[(A,S)]
}

case class QLearning[S,A](
    override val mdp: MDPSystem[S,A],
    override val initial: S,
    override val terminal: S => Boolean,
    val gamma: Double,
    val alpha: Double) extends QRL[S,A]{

  override def updateQ(s: S, qf: QFunction[S,A]): (S,QFunction[S,A]) = {
    val a = qf.actions maxBy {qf(s,_)}
    val (r,s2) = mdp.take(s,a)
    val q2 = (1-alpha)*qf(s,a) + alpha*(r + gamma * qf.value(s2))
    val qf2 = qf.update(s,a,q2)
    (s2,qf2)
  }

  @tailrec
  final override def runEpisodes(episodes: Int, qf: QFunction[S,A]): QFunction[S,A] = {
    @tailrec
    def runSingleEpisode(in: (S,QFunction[S,A])): (S,QFunction[S,A]) =
      if (terminal(in._1)) in else runSingleEpisode(updateQ(in._1,in._2))
    episodes match {
      case 0 => qf
      case _ => runEpisodes( episodes-1, runSingleEpisode( (initial,qf))._2)
    }
  }

  final override def run(qf: QFunction[S,A]): Stream[(A,S)] = {
    Stream.iterate( (initial,qf.actions.head,initial)){ case (_,a,s2) => val a2 = qf.detTake(s2); (s2,a2,mdp.take(s2,a2)._2) }
          .tail
          .takeWhile {case (s1,_,_) => !terminal(s1)}
          .map {case (_,a,s2) => (a,s2)}
  }
}

trait QLearningInMatrixEnvironment{
  val rows: Int
  val cols: Int
  
  type State = (Int,Int)
}

