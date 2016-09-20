// This is the text editor interface.
// Anything you type or change here will be seen by the other person in real time.
// To execute Scala, Do not remove the object named Solution that extends App.

// insert(n), n is an integer
// getMean(),
// getMedian(),

// 1 2 3
// getMean 2
// getMedian 2

// requirements
// O(1) constant space
// n [0, 999]


// Insert -> Array[1000] -> countain counts
 object MetricCalculator extends App {

    case class MetricCalc(){
        var sum: Int = 0
        var count: Int = 0

        var storage = Array.fill[Int](1000)(0)

        def insert(x: Int) = {
            storage(x) = storage(x) + 1
            sum = sum + x
            count = count + 1
        }


        def getMean() = if (count == 0 ) 0 else sum.toDouble/count

        def isCountEven() = count%2 == 0

        def getMedian() = {

            if (count == 0) throw new Exception("No Numbers present")
            else{

                val mid = count/2
                val init = 0.0

                def nextNonZeroIndex(ind: Int): Int = {
                    if (storage(ind) == 0) nextNonZeroIndex(ind+1)
                    else ind
                }

                def median(finalCount: Double, finalIndex: Double):Double = {

                    if(isCountEven()) {
                        if(finalCount.toInt - mid.toInt == 0)
                            (finalIndex + nextNonZeroIndex(finalIndex.toInt+1))/2
                        else finalIndex
                    }
                    else finalIndex
                }



                def getNumIndex(currCount: Double = init, currIndex: Double = init):Double = currCount match {
                    case _ if currCount >= mid => median(currCount, currIndex)
                    case _ => val x = currCount + storage(currIndex.toInt) //count of currIndex
                              if (x >= mid) median(x, currIndex)
                              else getNumIndex(x, currIndex + 1)
                }


                getNumIndex()
            }
        }

    }


    val metricCalc = MetricCalc()

    (0 until 1000).foreach(metricCalc.insert(_))

    println(s"mean -> ${metricCalc getMean()}")
    println(s"median -> ${metricCalc getMedian()}")


}
