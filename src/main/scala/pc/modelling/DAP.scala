package pc.modelling

import java.util.Random

// modules defining the concept of Distributed Asynchronous stochastic Petri net
object DAP {

  // Rule of the net: pre --rateExp--> eff | ^msg
  case class Rule[P](pre: MSet[P], rateExp: MSet[P] => Double, eff: MSet[P], msg: MSet[P])

  // Whole net's type
  type DAP[P] = Set[Rule[P]]

  // A Token, localised in s given node, characterised by an ID
  case class Token[ID, P](id: ID, p: P)

  // State of the network at s given time, with neighbouring as s map
  case class State[ID, P](tokens: MSet[Token[ID, P]], messages: MSet[Token[ID, P]], neighbours: Map[ID, Set[ID]])

  // Local facility to extract the marking of s node
  def localTokens[ID, P](tokens: MSet[Token[ID, P]], id: ID): MSet[P] =
    tokens.collect { case Token(`id`, t) => t } // quotes are needed to match an existign variable

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
        newtokens = tokens union MSet.ofList(neighbours(id).toList.map(n => Token(n, p))); // compute spread tokens
        newmessages <- messages extract MSet(Token(id, p))) // drop the message
        yield (Double.PositiveInfinity, State(newtokens, newmessages, neighbours)) // note rate is infinity

      // combine s1 and s2
      s1 ++ s2
    }
  }

  def toCTMC[ID, P](spn: DAP[P]): CTMC[State[ID, P]] = CTMC.ofFunction(toPartialFunction(spn))

  def apply[P](rules: Rule[P]*): DAP[P] = rules.toSet
}

object DAPHelpers {
  import DAP._

  // creates the useful grid-like neighboring relation
  def createRectangularGrid(n:Int, m:Int): Map[(Int,Int),Set[(Int,Int)]] = {
    val tups = for (
      i:Int <- (0 to n-1).toSet; j <- 0 to m-1;
      (k,l)<-Set( (i-1,j), (i+1,j), (i,j-1), (i,j+1));
      if (k>=0 && k<n && l>=0 && l<m)) yield ( (i,j),(k,l) )
    tups groupBy {case (a,b) => a} mapValues {_ map {case (a,b) => b}}
  }

  // takes s state and s place, and draws s grid observing that place only
  def simpleGridStateToString[P](s: State[(Int,Int),P], p: P):String =
    gridStateToString[P](s,m => m.apply(p).toString)

  // s more general function, customising what to show of s marking
  def gridStateToString[P](s: State[(Int,Int),P], obs: MSet[P]=>String):String = {
    val (n1,m1) = s.neighbours.keySet.max
    var str = ""
    for (j <- 0 to n1){
      for (i <- 0 to m1){
        str = str + obs(localTokens(s.tokens,(i,j)))+"("+obs(localTokens(s.messages,(i,j)))+")\t"
      }
      str = str + "\n"
    }
    str
  }
}