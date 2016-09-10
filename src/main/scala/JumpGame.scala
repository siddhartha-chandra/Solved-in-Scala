//Given an array of non-negative numbers, you are initially positioned as the first index of the array.
//Each element in the array represents your maximum jump length at that position.
//Determine if you are able to reach the last index

object JumpGame{
  def shortestPath(ls: Array[Int]) = {
    val n = ls.length
    val lastIndex = n-1
    require(ls.nonEmpty)
    require(ls.forall(_ >= 0))
    def internalShortestPath(path: String = "0",
                             curr: Int = 0,
                             acc: List[String]= Nil):List[String] = curr == lastIndex match {
       case true if curr ==0 => acc:+path
       case true => val p = path+"-"+curr.toString
                   acc:+p
       case _ => val jumps = (ls(curr) until 0 by -1).filter(_ + curr <= lastIndex)
                 jumps.map{j =>
                   val p = if(curr!= 0) path+ "-"+curr.toString
                           else path
                   internalShortestPath(p, curr + j, acc)
                 }.toList.flatten
    }

    val rawRes = internalShortestPath()
    rawRes match {
      case Nil => println("Not reachable!")
      case _ => val resWithLengths = rawRes.map(x=> (x, x.split('-').length))
                val minLength = resWithLengths.minBy(_._2)._2
                val res = resWithLengths.filter(_._2 == minLength).map(_._1)
                res
    }
  }
}
