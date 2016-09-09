import scala.math._

object RangePredictor{

  def getCategory(num: Int) = {
    if (num == 0) -1
    else if (num>=1 && num <= 10) 0
    else {
      val h1 = 2 * log10(num - 1).toInt - 1
      val h2 = 2 * log10(2*(num - 1)).toInt -1
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
      case -1 => println("Invalid")
      case _ if category%2 == 0 => pow(10, category/2).toInt
      case _ => 5 * pow(10, (category-1)/2).toInt
    }
    println(s"result -> $res")
  }

  def listen():Unit= {
    println("Input a number to classify or press x to exit: ")
    val input = scala.io.StdIn.readLine()
    input match {
      case "x" => ()
      case _ if input forall Character.isDigit => getOutput(getCategory)(input.toInt); listen()
      case _ => println("Error: Invalid input"); listen()
    }
  }

  def main(args: Array[String]) = listen()
}
