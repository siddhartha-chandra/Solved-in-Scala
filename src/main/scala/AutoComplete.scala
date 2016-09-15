import scala.collection.mutable._

object AutoComplete{
  case class TrieNode(elem: Char, var adj: Set[TrieNode] = Set[TrieNode]())
  var trieRoot = TrieNode('$', Set(TrieNode('#')))


  def addWordToTrie(ls: List[Char], tr: TrieNode = trieRoot):Unit = ls match {
      case Nil => tr.adj = tr.adj + TrieNode('#')
      case head::tail => if (tr.adj.exists(_.elem == head)) addWordToTrie(tail, tr.adj.find(_.elem == head).get)
                         else {
                           tr.adj = tr.adj + TrieNode(head)
                           addWordToTrie(tail, tr.adj.find(_.elem == head).get)
                         }
    }


  def buildTrie(ls: List[String]): Unit = {
      ls.foreach(word=> addWordToTrie(word.toList))
  }

  def getRemainingParts(tr: TrieNode) = {
    val s:Stack[(TrieNode,String)] = Stack()
    tr.adj.filterNot(_.elem == '#').foreach(x=>s.push((x,"")))

    def getRemainingPartsInternal(acc: List[String] = Nil):List[String] = s.size match {
      case 0 => acc
      case _ => val tn = s.pop
                if (tn._1.elem == '#') getRemainingPartsInternal(acc :+ tn._2)
                else {
                  tn._1.adj.foreach(x=>s.push((x,tn._2 + tn._1.elem)))
                  getRemainingPartsInternal(acc)
                }
    }
    getRemainingPartsInternal()
  }

  def getSuggestions(ls: List[Char], trie: TrieNode = trieRoot): List[String] = ls match {
    case Nil => getRemainingParts(trie)
    case h::t => if (trie.adj.exists(_.elem == h)) {
                  val res = trie.adj.find(_.elem == h).get
                  getSuggestions(t, res)
                  } else Nil
  }

  def listen():Unit = {
    val in = scala.io.StdIn.readLine
    in match{
      case "x" => ()
      case _ => println(getSuggestions(in.toList).map(in + _).mkString(" ")) ; listen()
    }
  }

  def main(args: Array[String]) = {
    val words = List("this", "is", "another", "tequila", "sunrise", "or", "sunset", "ink", "thought")
    buildTrie(words)
    println(trieRoot)
    listen()
  }
}
