import scala.math._

object NQueens{

 def placeQueens(n: Int) = {

   println(s"Solution for $n Queens")
   val globalArr = Array.fill[Int](n)(-1)
   var acc: List[Array[Int]] = Nil

   def isSafe(r:Int, c: Int) = {
     def notInDiag = (0 to r-1).forall(x=> abs(x-r) != abs(globalArr(x)-c))
     def notInCol  = (0 to r-1).forall(x=> globalArr(x)!= c)

      if (r==0) true
      else notInDiag && notInCol
   }
   def placeQueen(r: Int, c: Int) = globalArr(r) = c
   def placeQueensInternal(row: Int):Unit = {
     for(col <- 0 until n; if isSafe(row, col))
        {
          placeQueen(row, col)
          if(row == n-1) acc = acc:+ globalArr.clone
          else placeQueensInternal(row+1)
        }
   }
   placeQueensInternal(0)
   acc
 }

  def printArray(arr: Array[Int]) = println(arr.toList.mkString("-"))

  def main(args: Array[String]) = {
    val in = scala.io.StdIn.readLine()
    val res = placeQueens(in.toInt)
    res.foreach(sol => printArray(sol))
  }
}
