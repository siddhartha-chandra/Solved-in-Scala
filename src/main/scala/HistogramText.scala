import scala.concurrent._
import scala.concurrent.duration._

object HistogramText{

  var histogram = scala.collection.mutable.Map[Char, Int]()
  var lock = 0

  def updateHistogram(c: Char) = synchronized{
    if(histogram.contains(c))
      histogram(c) = histogram(c) + 1
    else histogram(c) = 1
  }

  def populateHistogram(word: String) = {
      word.toList.foreach{c=>updateHistogram(c)}
  }

  def populateHistogram(words: Array[String]) = {
    words.foreach{w=>
      w.toList.foreach{c=>updateHistogram(c)}
    }
  }

  def main(args: Array[String]) = {
    val in = scala.io.StdIn.readLine()
    val words = in.split(" ").toIterator

    import scala.concurrent.ExecutionContext.Implicits.global
    val resRaw = words.map{word=>
      Future(populateHistogram(word))
    }

    val res = Future.sequence(resRaw)
    Await.result(res, Duration.Inf)

    println(histogram)
  }
}
