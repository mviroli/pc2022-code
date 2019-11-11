package pc.rl

object QLearningMatrix {

  type Node = (Int,Int)
  object Action extends Enumeration {
    val LEFT,RIGHT,UP,DOWN = Value
    val actionToString = Map(LEFT->"<", RIGHT->">", UP->"^", DOWN->"v")
  }
  import Action._
  type Action = Action.Value

  type QRLType = QRLImpl[Node,Action]

  case class Configuration(
                            width: Int,
                            height: Int,
                            initial: Node,
                            terminal: PartialFunction[Node,Boolean],
                            reward:  PartialFunction[(Node,Action),Double],
                            jumps: PartialFunction[(Node,Action),Node],
                            gamma: Double,
                            alpha: Double,
                            epsilon: Double = 0.0,
                            v0: Double) extends QRLImpl[Node,Action] {

    def qEnvironment(): Environment = new Environment {
      override def take(s: Node, a: Action): (R, Node) = {
        // applies direction, without escaping borders
        val n2: Node = (s,a) match {
          case ((n1,n2),UP) => (n1,(n2-1) max 0)
          case ((n1,n2),DOWN) => (n1,(n2+1) min (height-1))
          case ((n1,n2),LEFT) => ((n1-1) max 0 ,n2)
          case ((n1,n2),RIGHT) => ((n1+1) min (width-1),n2)
          case _ => ???
        }
        // computes rewards, and possibly a jump
        (reward.apply((s,a)),
          jumps.orElse[(Node,Action),Node]{case _=>n2}.apply((s,a)))
      }
    }

    def qFunction = QFunction(Action.values,v0,terminal)

    def qSystem = QSystem(
      environment = qEnvironment(),
      initial,
      terminal
    )

    def makeLearningInstance() = QLearning(
      qSystem, gamma, alpha, epsilon, qFunction
    )

    def show[E](v: Node=>E, formatString:String): String = {
      (for (row <- 0 until width;
           col <- 0 until height)
        yield (formatString.format(v((col,row))) + (if (col == height-1) "\n" else "\t"))).mkString("")
    }
  }

}

object TryQLMatrix extends App {

  import QLearningMatrix._
  import QLearningMatrix.Action._

  val cfg: QLearningMatrix.Configuration = Configuration(
    width = 5,
    height = 5,
    initial = (0,0),
    terminal = {case _=>false},
    reward = { case ((1,0),DOWN) => 10; case ((3,0),DOWN) => 5; case _ => 0},
    jumps = { case ((1,0),DOWN) => (1,3); case ((3,0),DOWN) => (3,2) },
    gamma = 0.9,
    alpha = 0.5,
    epsilon = 0.05,
    v0 = 1
  )

  val q0 = cfg.qFunction
  println(cfg.show(q0.vFunction,"%2.2f"))
  val q1 = cfg.makeLearningInstance().runEpisodes(100,100,q0)
  println(cfg.show(q1.vFunction,"%2.2f"))
  println(cfg.show(s => actionToString(q1.bestPolicy(s)),"%7s"))

}