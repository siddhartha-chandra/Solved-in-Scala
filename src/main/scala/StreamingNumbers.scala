object StreamingNumbers{

  case class StreamCalculator(){
    var sum: Int = 0
    var count: Int = 0
    val storage = Array.fill[Int](1000)(0)

    def insert(x: Int) = {
      storage(x) = storage(x) + 1
      sum= sum + x
      count= count + 1
    }

    def mean() = (sum.toDouble)/count

    def isEven(x: Int = count) = count%2 == 0

    def median() = {
      val mid = count/2

      println(s"get $mid number")

      def getNextNonEmpty(idx: Double):Double = idx match {
        case _ if storage(idx.toInt)!=0 => idx
        case _ => getNextNonEmpty(idx+1)
      }


      def getMedianResult(finalIndex: Double, accCount: Int) = {
        if (isEven()){
          if(accCount - mid == 0){
            (finalIndex + getNextNonEmpty(finalIndex+1))/2
          } else finalIndex
        }else finalIndex
      }

      def getMedianInternal(idx: Double = 0.0, currCount: Int = 0):Double = currCount match {
        case _ if currCount >= mid => getMedianResult(idx, currCount)
        case _ => val updatedCount = currCount + storage(idx.toInt)
                  if (updatedCount >= mid) getMedianResult(idx, updatedCount)
                  else getMedianInternal(idx+1, updatedCount)
      }

      if (count>1) getMedianInternal()
      else if (count == 1) this.sum
      else 0

    }

  }

  def listen(s: StreamCalculator):Unit = {
    val in = scala.io.StdIn.readLine
    val numTry = scala.util.Try(in.toInt)
    numTry match {
      case scala.util.Success(x) => s insert (x);
                                    println("Inserted!")
                                    println(s"total: ${s.sum}")
                                    println(s"numbers inserted: ${s.count}")
                                    listen(s)
      case _ if in == "median" => val res = s median()
                                  println(s"median -> $res"); listen(s)
      case _ if in == "mean" => val res = s mean()
                                println(s"mean -> $res"); listen(s)
      case _ if in == "exit" => ()
      case _ => listen(s)
    }
  }

  def main(args: Array[String]) = {
    val s = StreamCalculator()
    listen(s)

  }
}
