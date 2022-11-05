package pc.modelling

import pc.utils.MSet

object PetriNet:
  // pre-conditions, effects, inhibition
  case class Trn[P](in: MSet[P], out: MSet[P], inh: MSet[P])
  type PetriNet[P] = Set[Trn[P]]
  type Marking[P] = MSet[P]

  // factory of A Petri Net
  def apply[P](transitions: Trn[P]*): PetriNet[P] = transitions.toSet

  // factory of A System
  def toSystem[P](pn: PetriNet[P]): System[Marking[P]] = m =>
    for
      Trn(cond, eff, inh) <- pn
      if m disjoined inh
      out <- m extract cond
    yield out union eff

  // fancy syntax
  extension [P](self: MSet[P])
    def ~~> (y: MSet[P]) = Trn(self, y, MSet[P]())
  extension [P](self: Trn[P])
    def ^^^ (z: MSet[P]) = self.copy(inh = z)