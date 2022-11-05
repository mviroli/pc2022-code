package pc.modelling

// The definition of a Rewrite System, as a function: S => Set[S]
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

    // complete paths with length '<= depth' (could be optimised)
    def completePathsUpToDepth(s: S, depth:Int): Seq[Path[S]] =
      (1 to depth).to(LazyList) flatMap (paths(s,_)) filter (complete(_))

object System: // Our factory of Systems

  // The most general case, an intensional one
  def ofFunction[S](f: PartialFunction[S,Set[S]]): System[S] =
    s => f.applyOrElse(s, x => Set[S]())

  // Extensional specification
  def ofRelation[S](rel: Set[(S,S)]): System[S] =
    ofFunction(s => rel collect {case (`s`, s2) => s2})

  // Extensional with varargs
  def ofTransitions[S](rel: (S,S)*): System[S] =
    ofRelation(rel.toSet)