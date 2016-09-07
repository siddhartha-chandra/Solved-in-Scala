object MergeSort{
  def sort(ls: List[Int]): List[Int] = {
    def merge(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
      case (Nil, Nil) => Nil
      case (_, Nil) => l
      case (Nil, _) => r
      case (h1::t1, h2::t2) =>
        if (h1 < h2) h1 :: merge(t1, r)
        else h2 :: merge(l, t2)
    }

    val halfLen = ls.length /2
    halfLen match {
      case 0 => ls
      case _ =>
        val (left, right) = ls splitAt halfLen
        merge(sort(left), sort(right))
    }
  }
}
