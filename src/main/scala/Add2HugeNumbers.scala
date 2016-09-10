//add 2 huge numbers represented by linked list. Each linked list element represents a 4 digit number:
//linked list1 : 8798 -> 8765 -> 1243 -> 9856 -> 8888 -> 0914
//linked list 2: 8710 -> 5634 -> 1276 -> 8123 -> 1354 -> 9876
//output: ................-> ............. ..-> 7980->0243 -> 0790

object Add2HugeNumbers{

  val NoCarry = 0
  val FixedLength = 4

  def fourDigitPerTerm(ls: List[Int]): Boolean = ls match {
    case Nil => true
    case h::t => if (h.toString.length == FixedLength) fourDigitPerTerm(t) else false
  }

  def addNumbers(ls1: List[Int], ls2: List[Int]) = {
    require(fourDigitPerTerm(ls1++ls2))
    val ls1Reversed = ls1.reverse
    val ls2Reversed = ls2.reverse

    def internalAddNumbers(ls1: List[Int],
                           ls2: List[Int],
                           carry: Int = NoCarry,
                           acc: List[String] = Nil): List[String] = (ls1, ls2) match {

      case (Nil, Nil) if carry == NoCarry => acc
      case (Nil, Nil) => s"000$carry"::acc
      case (_, _) => val elem = carry +
                                ls1.headOption.getOrElse(0) +
                                ls2.headOption.getOrElse(0)
                     val elemLength = elem.toString.length
                     val (c, addedElem) =
                             if (elemLength > FixedLength) (1, elem.toString.tail)
                             else (NoCarry, elem.toString)
                     internalAddNumbers(
                       if(ls1.isEmpty) Nil else ls1.tail,
                       if(ls2.isEmpty) Nil else ls2.tail, c,
                       "0"*(FixedLength-elemLength)+addedElem::acc)
    }
    internalAddNumbers(ls1Reversed, ls2Reversed)
  }

}
