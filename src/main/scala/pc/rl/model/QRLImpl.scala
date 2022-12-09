package pc.rl.model

import scala.annotation.tailrec

trait QRLImpl[S, A] extends QRL[S, A]:

  // A Map-based implementation, with defaults for terminal and unexplored states
  case class QFunction(
                        override val actions: Set[A],
                        v0: R = 0.0,
                        terminal: S => Boolean = (s: S) => false,
                        terminalValue: Double = 0.0) extends Q :
    val map: collection.mutable.Map[(S, A), R] = collection.mutable.Map()

    override def apply(s: S, a: A) = if (terminal(s)) terminalValue else map.getOrElse(s -> a, v0)

    override def update(s: S, a: A, v: Double): Q =
      map += ((s -> a) -> v)
      this

    override def toString = map.toString

  case class QSystem(
                      override val environment: Environment,
                      override val initial: S,
                      override val terminal: S => Boolean) extends System :

    final override def run(p: Policy): LazyList[(A, S)] =
      val a0 = p(initial)
      LazyList.iterate((initial, a0, initial)) { case (_, a, s2) => val a2 = p(s2); (s2, a2, environment(s2, a2)._2) }
        .tail
        .takeWhile { case (s1, _, _) => !terminal(s1) }
        .map { case (_, a, s2) => (a, s2) }

  case class QLearning(
      override val system: QSystem,
      override val gamma: Double,
      override val alpha: Double,
      override val epsilon: Double,
      override val q0: Q) extends LearningProcess :

    override def updateQ(s: S, qf: Q): (S, Q) =
      val a = qf.explorativePolicy(epsilon)(s)
      val (r, s2) = system.environment(s, a)
      val vr = (1 - alpha) * qf(s, a) + alpha * (r + gamma * qf.vFunction(s2))
      val qf2 = qf.update(s, a, vr)
      (s2, qf2)

    @tailrec
    final override def learn(episodes: Int, episodeLength: Int, qf: Q): Q =
      @tailrec
      def runSingleEpisode(in: (S, Q), episodeLength: Int): (S, Q) =
        if episodeLength == 0 || system.terminal(in._1)
          then in
          else runSingleEpisode(updateQ(in._1, in._2), episodeLength - 1)

      if episodes == 0
        then qf
        else learn(
          episodes - 1,
          episodeLength,
          runSingleEpisode((system.initial, qf), episodeLength)._2)