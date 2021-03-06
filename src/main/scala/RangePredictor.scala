import scala.math._                                 // import scala.math._

//Given the following table format:
//------------------------
//    a       b       y
//    1       10      1
//    11      50      5
//    51      100     10
//    101     500     50
//    501     1000    100
//    1001    5000    5000
//    ....
//    .....

//input: any natural number
//output: Category: Category of the number
//        Result: Result of the category

//eg. input:7
//    output:category -> 0; result -> 1
//    input:555
//    output:category -> 4; result -> 500
//    input:1001
//    output:category -> 5; result -> 1000

object RangePredictor {

  def getCategory(num: Int) = {
    if (num == 0) -1
    else if (num >= 1 && num <= 10) 0
    else {
      val h1 = 2 * log10(num - 1).toInt - 1
      val h2 = 2 * log10(2 * (num - 1)).toInt - 1
      h2 == h1 match {
        case true => h1
        case false => h2 - 1
      }
    }
  }

  def getOutput(f: Int => Int)(num: Int) = {
    val category = f(num)
    println(s"category -> $category")
    val res = category match {
      case -1 => category
      case _ if category % 2 == 0 => pow(10, category.toDouble / 2).toInt
      case _ => 5 * pow(10, (category - 1) / 2).toInt
    }
    if (res == -1) println(s"result -> Invalid")
    else println(s"result -> $res")
  }

  def directOutput(x:Int) = x match {
    case 0 => -1
    case _ if x>0 && x<= 10 => 1
    case _ => val logNum = log10(x.toDouble-1).toInt
              val res = pow(10, logNum.toDouble).toInt
              if (x <= 5*res) res/2
              else res
  }

  def listen(): Unit = {
    println("Input a number to classify or press x to exit: ")
    val input = scala.io.StdIn.readLine()
    input match {
      case "x" => ()
      case _ if input forall Character.isDigit =>
        //getOutput(getCategory)(input.toInt); listen()
        println(directOutput(input.toInt)); listen()
      case _ => println("Error: Invalid input"); listen()
    }
  }

  def main(args: Array[String]) = listen()
}
