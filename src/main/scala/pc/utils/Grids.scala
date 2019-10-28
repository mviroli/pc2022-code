package pc.utils

object Grids {
  // creates the useful grid-like neighboring relation
  def createRectangularGrid(n:Int, m:Int): Map[(Int,Int),Set[(Int,Int)]] = {
    val tups = for (
      i:Int <- (0 to n-1).toSet; j <- 0 to m-1;
      (k,l)<-Set( (i-1,j), (i+1,j), (i,j-1), (i,j+1));
      if (k>=0 && k<n && l>=0 && l<m)) yield ( (i,j),(k,l) )
    tups groupBy {case (a,b) => a} mapValues {_ map {case (a,b) => b}}
  }

  // pretty printing A grid
  def gridLikeToString[S](rows: Int, cols: Int, obs: (Int,Int)=>String):String =
    (for (j <- 0 to rows;
          str = (for (i <- 0 to cols; s = obs(i,j)) yield s).mkString("\t")
          ) yield str
      ).mkString("\n")

}