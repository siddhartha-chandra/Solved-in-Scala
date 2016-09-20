import scala.reflect.Manifest



object MatrixSpiralTraverser{

  type Matrix = Array[Array[Int]]

  def show(m:Matrix) = println(m.map(_.mkString(" ")).mkString("\n"))
  def genMatrix(dim: Int) = Array.tabulate[Int](dim,dim)((x,y)=> dim*x + y + 1)

  // def genMatrix[T:Manifest](dim: Int)(implicit mf: Manifest[T]) = {
  //   val res =  mf match {
  //     case m if m == manifest[Int] => Array.tabulate[Int](dim,dim)((x,y)=> dim*x + y + 1)
  //     case m if m == manifest[Char] => Array.tabulate[Char](dim,dim)((x,y) => (dim*x + y + 97).toChar)
  //     case _ => throw new Exception("Unsupported type!")
  //   }
  //   res
  // }


  def spiralTraverser(m: Matrix) = {
    def length = m.length
    def isEven() = length%2 ==0
    val finalLoop = if (isEven()) (length-1)/2 else length/2

    def l2r(start:Int)(stop:Int)(loopCount: Int) = (start, loopCount)
    def u2d(start:Int)(stop:Int)(loopCount: Int) = (loopCount, stop)
    def r2l(start:Int)(stop:Int)(loopCount: Int) = (stop, stop-loopCount)
    def d2u(start:Int)(stop:Int)(loopCount: Int) = (stop-loopCount, start)

    def getTraversal(start: Int, stop: Int, traversalType: Int => (Int, Int)) = {
      (start until stop).map{x =>
        m(traversalType(x)._1)(traversalType(x)._2)
      }
    }

    def getLoopTraversal(start: Int, stop: Int) = {
      val lr = getTraversal(start, stop, l2r(start)(stop) _)
      val ud = getTraversal(start, stop, u2d(start)(stop) _)
      val rl = getTraversal(start, stop, r2l(start)(stop) _)
      val du = getTraversal(start, stop, d2u(start)(stop) _)
      println(lr)
      println(ud)
      println(rl)
      println(du)
      lr ++ ud ++ rl ++ du
    }

    def spiralTraverseInternal(currLoop: Int = 0,
                               acc: List[Int] = Nil):Unit = currLoop match {

      case `finalLoop` if isEven() =>  println(acc )
      case `finalLoop` =>  println(acc:+ m(finalLoop)(finalLoop))
      case _ =>  val start = currLoop
                 val stop = length - (currLoop + 1)
                 val ls = getLoopTraversal(start, stop)
                 spiralTraverseInternal(currLoop+1, acc++ls)
    }
    println(s"finalLoop $finalLoop")
    val res = spiralTraverseInternal()
  }

  def main(args: Array[String]) = {
    val evenMatrix = genMatrix(4)
    val oddMatrix = genMatrix(5)

    show(evenMatrix)
    spiralTraverser(evenMatrix)
    show(oddMatrix)
    spiralTraverser(oddMatrix)

  }
}
