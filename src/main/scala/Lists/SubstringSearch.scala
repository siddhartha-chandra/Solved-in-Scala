import scala.io.Source

object SubstringSearch{

    def hasSubstring(text: Array[Char], pattern: Array[Char]) = {
        val txtLen = text.length
        val patternLen = pattern.length
        val table = Array.ofDim[Int](patternLen)

        def populateTable() = {
            table(0) = -1
            def populateTableInternal(pos: Int, cnd: Int):Unit = pos match {
                case `patternLen` => ()
                case _ =>  if (pattern(pos-1) == pattern(cnd)) {
                                table(pos) = cnd + 1
                                populateTableInternal(pos + 1, cnd + 1)}
                           else if (cnd > 0){
                               table(pos) = 0
                               populateTableInternal(pos, table(cnd))}
                           else {
                               table(pos) = 0
                               populateTableInternal(pos + 1, cnd)}

            }
            if (patternLen > 1) {table(1) = 0; populateTableInternal(2, 0)}
        }

        def hasSubstringInternal(m:Int, i:Int):String = (m, i) match {
            case (_, `patternLen`) => "YES"
            case (`txtLen`, _) => "NO"
            case (_, _) if m + i >= txtLen => "NO"
            case (_, _) => if (text(m + i) == pattern(i)) hasSubstringInternal(m, i+1)
                           else if (table(i) > -1) hasSubstringInternal(m+i- table(i), table(i))
                           else hasSubstringInternal(m + 1, 0)
        }
        populateTable()
        println("done!")
        hasSubstringInternal(0, 0)
      }

    def main(args: Array[String]) {
        val currentDirectory = new java.io.File(".").getCanonicalPath
        val fileName = "test_SubstringSearch.txt"
        val filePath = s"$currentDirectory/src/main/resources/$fileName"

        val lines = Source.fromFile(filePath).getLines()
        val inputArray = for(i <- 1 to lines.next.toInt) yield (lines.next, lines.next)
        inputArray.foreach(x=> println(hasSubstring(x._1.toArray, x._2.toArray)))
    }

}
