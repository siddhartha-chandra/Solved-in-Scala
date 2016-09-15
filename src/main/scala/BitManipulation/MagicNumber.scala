object MagicNumber{

  def getMagicNumber(num: Int) = {
    def getMagicNumberInternal(rem: Int, counter: Int = 1, acc: Int = 0): Int = rem match {
      case 0 => acc
      case _ => val pwr=  1 & rem
                val res = pwr * scala.math.pow(5, pwr * counter).toInt
                getMagicNumberInternal(rem  >> 1, counter+1, acc + res)
    }
    getMagicNumberInternal(num)
  }

  def main (args: Array[String]) = {
      (1 to 20).foreach(x=> println(x + " -> " + getMagicNumber(x)))
  }

}
