package pc.rl.model

object QMatrix:

  type Node = (Int, Int)

  enum Action:
    case LEFT, RIGHT, UP, DOWN
  object Action:
    val actionToString = Map(LEFT -> "<", RIGHT -> ">", UP -> "^", DOWN -> "v")

  import Action.*

  case class Facade(
                     width: Int,
                     height: Int,
                     initial: Node,
                     terminal: PartialFunction[Node, Boolean],
                     reward: PartialFunction[(Node, Action), Double],
                     jumps: PartialFunction[(Node, Action), Node],
                     gamma: Double,
                     alpha: Double,
                     epsilon: Double = 0.0,
                     v0: Double) extends QRLImpl[Node, Action]:

    def qEnvironment(): Environment = (s: Node, a: Action) =>
        // applies direction, without escaping borders
        val n2: Node = (s, a) match
          case ((n1, n2), UP) => (n1, (n2 - 1) max 0)
          case ((n1, n2), DOWN) => (n1, (n2 + 1) min (height - 1))
          case ((n1, n2), LEFT) => ((n1 - 1) max 0, n2)
          case ((n1, n2), RIGHT) => ((n1 + 1) min (width - 1), n2)
          case _ => ???
        // computes rewards, and possibly a jump
        (reward.apply((s, a)), jumps.orElse[(Node, Action), Node](_ => n2).apply((s, a)))

    def qFunction = QFunction(Action.values.toSet, v0, terminal)

    def qSystem = QSystem(
      environment = qEnvironment(),
      initial,
      terminal
    )

    def makeLearningInstance() = QLearning(
      qSystem, gamma, alpha, epsilon, qFunction
    )

    def show[E](v: Node => E, formatString: String): String =
      (for
        y <- 0 until height
        x <- 0 until width
        s1 = formatString.format(v((x, y)))
        s2 = if x == width - 1 then "\n" else "\t"
      yield s1 + s2).mkString("")