import scala.collection.mutable._

object BracketCombinations{

  def bracketCombinations(n: Int) = {
    def bracketCombinationsInternal(currN: Int,
                                    balanceStack: Stack[Char] = new Stack[Char],
                                    curr: String = "",
                                    acc: List[String] = Nil): List[String] =
    currN match {
      case 0 if balanceStack.isEmpty => acc:+curr

      case 0 if balanceStack.nonEmpty=>
                balanceStack.pop
                bracketCombinationsInternal(currN, balanceStack, curr+")", acc)

      case _ if balanceStack.isEmpty=>
                balanceStack.push('(')
                bracketCombinationsInternal(currN-1, balanceStack, curr+"(", acc)

      case _ if balanceStack.nonEmpty=>
                val newStack = new Stack[Char]
                balanceStack.foreach(newStack.push(_))
                newStack.push('(')
                balanceStack.pop
                bracketCombinationsInternal(currN-1, newStack, curr+"(", acc) ++
                bracketCombinationsInternal(currN, balanceStack, curr+")", acc)

     }
  bracketCombinationsInternal(n)
  }
}
