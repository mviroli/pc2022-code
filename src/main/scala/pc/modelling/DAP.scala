package pc.modelling

import java.util.Random
import pc.utils.{MSet,Grids}

// modules defining the concept of Distributed Asynchronous stochastic Petri net
object DAP {

  // Rule of the net: pre --rateExp--> eff | ^msg
  case class Rule[P](pre: MSet[P], rateExp: MSet[P] => Double, eff: MSet[P], msg: MSet[P])

  // Whole net's type
  type DAP[P] = Set[Rule[P]]

  // A Token, localised in A given node, characterised by an ID
  case class Token[ID, P](id: ID, p: P)

  // state of the network at A given time, with neighbouring as A map
  case class State[ID, P](tokens: MSet[Token[ID, P]], messages: MSet[Token[ID, P]], neighbours: Map[ID, Set[ID]])

  // Local facility to extract the marking of s node
  def localTokens[ID, P](tokens: MSet[Token[ID, P]], id: ID): MSet[P] =
    tokens.collect { case Token(`id`, t) => t } // quotes are needed to match an existing variable

  // Here's the implementation of operational semantics
  def toPartialFunction[ID, P](spn: DAP[P]): PartialFunction[State[ID, P], Set[(Double, State[ID, P])]] = {
    case State(tokens, messages, neighbours) => {
      // we first try to apply rules
      val s1 = for (
        Rule(pre, rateExp, eff, msg) <- spn; // get any rule
        nodeId <- neighbours.keySet; // get any node
        out <- tokens extract pre.map(Token(nodeId, _)); // checks if that node matches pre
        newtokens = out union eff.map(Token(nodeId, _)); // generate new tokens
        newmessages = messages union msg.map(Token(nodeId, _)); // generate new messages
        rate = rateExp(localTokens(tokens, nodeId))) // compute rate
        yield (rate, State(newtokens, newmessages, neighbours))

      // we should also spread messages
      val s2 = for (
        Token(id: ID, p: P) <- messages.asList.toSet; // get any message
        newtokens = tokens union MSet.ofList(neighbours(id).toList.map(Token(_, p))); // compute spread tokens
        newmessages <- messages extract MSet(Token(id, p))) // drop the message
        yield (Double.PositiveInfinity, State(newtokens, newmessages, neighbours)) // note rate is infinity

      // combine s1 and s2
      s1 ++ s2
    }
  }

  def toCTMC[ID, P](spn: DAP[P]): CTMC[State[ID, P]] = CTMC.ofFunction(toPartialFunction(spn))

  def apply[P](rules: Rule[P]*): DAP[P] = rules.toSet
}

object DAPGrid {
  import DAP._

  // prints a grid of counting of p's tokens in the form of tokens(messages)
  def simpleGridStateToString[P](s: State[(Int,Int),P], p: P):String =
    Grids.gridLikeToString(s.neighbours.keySet.max._1,s.neighbours.keySet.max._1,
      (i,j) =>  localTokens(s.tokens,(i,j)).apply(p).toString +
               "("+localTokens(s.messages, (i,j)).apply(p).toString+")"
    )
}