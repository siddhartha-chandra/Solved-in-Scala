object InserKInOrderedList{

  trait LinkedListNode[+A]
  case class Cons[A](head: A, tail: LinkedListNode[A] = Empty) extends LinkedListNode[A]
  case object Empty extends LinkedListNode[Nothing]

  def combine(ls1: LinkedListNode[Int], ls2: LinkedListNode[Int]) = {
    def combineInternal(remLs1: LinkedListNode[Int],
      remLs2: LinkedListNode[Int]):LinkedListNode[Int] = (remLs1, remLs2) match {
      case (Cons(h,t), _) => Cons(h, combineInternal(t,remLs2))
      case (Empty, _) => remLs2
    }
    combineInternal(ls1,ls2)
  }

  def insert(k:Int, ls:LinkedListNode[Int]):LinkedListNode[Int] = {
    def insertInternal(rem: LinkedListNode[Int],
      acc:LinkedListNode[Int] = Empty):LinkedListNode[Int] = rem match {
      case Cons(h,Empty) if k>h => combine(acc,Cons(k,Empty))
      case Cons(h,t) if k<=h => combine(acc,Cons(k, Cons(h,t)))
      case Cons(h,t) => insertInternal(t, combine(acc, Cons(h,Empty)))
      case _ => acc
    }
    ls match {
      case Empty => Cons(k, Empty)
      case _ => insertInternal(ls)
    }
  }

  def main(args: Array[String]):Unit = {
    val ls = Cons(3, Cons(5, Cons(8, Empty)))
    val ls1 = insert(7,ls)
    val ls2 = insert(1,ls1)
    val ls3 = insert(9,ls2)
    println(ls3)
  }
}
