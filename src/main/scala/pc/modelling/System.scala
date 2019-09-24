package pc.modelling

// Basically the definition of a Rewrite System
trait CoreSystem[A] {

  def next(a: A): Set[A]
}

// Basic analysis helpers
trait System[A] extends CoreSystem[A] {

  def normalForm(a: A): Boolean =
    next(a).isEmpty

  def complete(p: List[A]): Boolean =
    normalForm(p.last)

  def paths(a: A, depth: Int): Stream[List[A]] = {
    if (depth == 0)
      Stream()
    else if (depth == 1 || normalForm(a))
      Stream(List(a))
    else
      for (path <- paths(a, depth - 1);
           next <- next(path.last)) yield (path :+ next)
  }

  // an infinite stream: might loop, use with care!
  def completePaths(a: A): Stream[List[A]] =
    Stream.iterate(1)(_+1) flatMap (paths(a,_)) filter (complete(_))
}

object System { // Our factory of Systems

  // The most general case, an intensional one
  def ofFunction[A](f: PartialFunction[A,Set[A]]): System[A] =
    new CoreSystem[A] with System[A] {
      override def next(a: A) = f.applyOrElse(a,(x: A)=>Set[A]())
    }

  // Extensional specification
  def ofRelation[A](r: Set[(A,A)]): System[A] = ofFunction{
    case a => r filter (_._1 == a) map (_._2)
  }

  // Extensional with varargs.. note binary tuples can be defined by a->b
  def ofTransitions[A](r: (A,A)*): System[A] = ofRelation(r.toSet)
}