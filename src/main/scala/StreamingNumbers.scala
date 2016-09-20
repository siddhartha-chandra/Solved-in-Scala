import scala.collection._
import scala.util.Random

object StreamingNumbers{
object MinOrder extends Ordering[Int] {
         def compare(x:Int, y:Int) = y compare x
       }

case class TopNTracker(n: Int){
  val minHeap = mutable.PriorityQueue.empty(MinOrder)
  def process(i:Int):Unit = {
    val min = minHeap.headOption

    if(min.exists(x=> x<i) || minHeap.length<n){
      if(minHeap.length>=n) minHeap.dequeue()
      minHeap.+=(i)
    }
  }

  def getTop():Seq[Int] = {
    minHeap.take(this.n).toSeq
  }
}

def main(args: Array[String]) = {
  val topNTracker = TopNTracker(5)
  val ls = (0 to 10).map(x=> Random.nextInt(999))
  println(ls)

  ls.foreach{s =>
    topNTracker process(s)
  }
  val res = (topNTracker getTop()).sortBy(-_)
  res.foreach(println)
}


}
