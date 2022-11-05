package pc.modelling

// Basically the definition of a Rewrite System
// essentially, a: S => Set[S]
trait System[S]:
  def next(a: S): Set[S]

// Basical analysis helpers
object SystemAnalysis:

  type Path[S] = List[S]

  extension [S](system: System[S])
    def normalForm(s: S): Boolean = system.next(s).isEmpty

    def complete(p: Path[S]): Boolean = normalForm(p.last)

    // paths of exactly length `depth`
    def paths(s: S, depth: Int): Seq[Path[S]] = depth match
      case 0 => LazyList()
      case 1 => LazyList(List(s))
      case _ =>
        for
          path <- paths(s, depth - 1)
          next <- system.next(path.last)
        yield path :+ next

    // complete path with length '<= depth' (could be optimised)
    def completePathsUpToDepth(s: S, depth:Int): Seq[Path[S]] =
      LazyList.iterate(1)(_+1) take (depth) flatMap (paths(s,_)) filter (complete(_))

    // an infinite stream: might loop, use with care!
    def completePaths(s: S): Seq[Path[S]] =
      LazyList.iterate(1)(_+1) flatMap (paths(s,_)) filter (complete(_))

object System: // Our factory of Systems

  // The most general case, an intensional one
  def ofFunction[S](f: PartialFunction[S,Set[S]]): System[S] =
    s => f.applyOrElse(s, (x: S) => Set[S]())

  // Extensional specification
  def ofRelation[S](rel: Set[(S,S)]): System[S] =
    ofFunction(s => rel collect {case (`s`, s2) => s2})

  // Extensional with varargs.. note binary tuples can be defined by s->b
  def ofTransitions[S](rel: (S,S)*): System[S] =
    ofRelation(rel.toSet)