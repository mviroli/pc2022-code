package lab.demo

import it.unibo.scafi.incarnations.BasicAbstractIncarnation

object Incarnation extends BasicAbstractIncarnation
import Incarnation._ //import all stuff from an incarnation

class Program1 extends AggregateProgram {
  override def main(): Int = 10
}

class Program2 extends AggregateProgram {
  override def main(): Double = 10 + 20
}

class Program3 extends AggregateProgram {
  override def main(): String = "Hello"
}

class Program4 extends AggregateProgram {
  override def main(): List[Any] = List(1.0, 2.0, 3.0)
}

class Program5 extends AggregateProgram {
  override def main(): Int = rep(0)(_ + 1)
}

class Program6 extends AggregateProgram {
  override def main(): Int = foldhood(0)(_ + _)(nbr(1))
}