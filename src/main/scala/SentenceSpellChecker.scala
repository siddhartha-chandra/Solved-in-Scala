// input: sentence that is a string
// output: true if all the words in the sentence are spelt correctly

object SentenceSpellChecker{
  case class TrieNode(elem: Char, adj: Set[TrieNode] = Set[TrieNode]())

  type Word = String
  type Dictionary = TrieNode
  //   $
  //   |
  //   a
  //   |   \
  //   t    #

  val dictionaryA = TrieNode('a', Set(TrieNode('t'), TrieNode('#')))
  val dictionaryC = TrieNode('c',
      Set(
        TrieNode('a',
          Set(TrieNode('b'), TrieNode('t')))))

  val dictionaryF = TrieNode('f',
      Set(
        TrieNode('o',
          Set(TrieNode('r'), TrieNode('x')))))

  val dictionaryI = TrieNode('i',
      Set(TrieNode('s'),TrieNode('n'), TrieNode('t')))

  val dictionaryN =
    TrieNode('n',
      Set(
        TrieNode('i',
          Set(TrieNode('c',
            Set(TrieNode('e')))))))

  val dictionaryT = TrieNode('t',
      Set(
        TrieNode('h',
          Set(TrieNode('i',
            Set(TrieNode('s')))))))


  val dictionarySet = Set(dictionaryA,
    dictionaryC,
    dictionaryF,
    dictionaryI,
    dictionaryN,
    dictionaryT)

  val DictionaryTrie = TrieNode('$', dictionarySet)



  def getNextNode(char:Char, ref: TrieNode) = {
      val ls = ref.adj
      ls.find(_.elem == char)
  }

  def lookup[A](chars: List[Char], ref: TrieNode): Boolean = {
    chars match {
      case Nil if ref.adj.isEmpty || ref.adj.exists(_.elem == '#') => true
      case Nil => false
      case h::t => val nextNode = getNextNode(h, ref)
                   nextNode match{
                      case None => false
                      case Some(res) =>  lookup(t, res)}
    }
  }

  def spellCheck(sentence: String) = {
      sentence.split(" ").forall(word => lookup(word.toLowerCase().toList, DictionaryTrie))
  }


  def main(args: Array[String]):Unit = {
    val sentence = scala.io.StdIn.readLine()
    println(s"Found in dictionary: ${spellCheck(sentence)}")
  }
}
