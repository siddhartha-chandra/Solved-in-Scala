import scala.collection.mutable._
import scala.util._

object ReversePolish{

  def isNumber(s:String) = {
    Try(s.toInt) match {
      case Success(x) => true
      case _ => false
    }
  }

  def isValidOperator(s: String) = List("/","*","+","-").contains(s)

  def operate(o: String, pre: Int, post: Int):Int = o match {
    case "/" => pre/post
    case "*" => pre * post
    case "-" => pre - post
    case "+" => pre + post
    case _ => throw new Exception("Invalid Operator!")
  }

  def evaluate(ls: List[String]):Int = {
    val s = Stack[String]()
    def evaluateInternal(rem: List[String]):Int = rem match {
      case Nil => if (s.size == 1 && isNumber(s.top)) s.pop.toInt
                  else throw new Exception("Invalid expression 1")
      case h::t if isNumber(h) => s.push(h); println(s); evaluateInternal(t)
      case h::t => if (isValidOperator(h) && s.size>=2){
                      val post = s.pop
                      val pre = s.pop
                      if (isNumber(post) && isNumber(pre)) {
                        val res = operate(h,pre.toInt,post.toInt)
                        s.push(res.toString)
                        println(s)
                        evaluateInternal(t)
                      } else throw new Exception("Invalid expression 2")
                    } else throw new Exception("Invalid expression 3")
    }
    println(s)
    evaluateInternal(ls)
  }

  def main(args: Array[String]) = {
      val listOfLists = List(List("2","1","+", "3", "*"), List("4","13","5", "/", "+"))
      listOfLists.foreach{ls=>
        val res = evaluate(ls)
        println(res)
      }

  }

}
