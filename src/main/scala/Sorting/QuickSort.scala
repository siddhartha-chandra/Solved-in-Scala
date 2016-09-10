object QuickSort{
  def sort(xs: List[Int]): List[Int] = {
    if (xs.length <= 1) xs
    else {
      val pivot = xs(xs.length / 2)
      sort(xs.filter(x=> pivot > x)) ++
        xs.filter (_ == pivot) ++
        sort(xs.filter(x=> pivot < x))
    }
  }
}
