import scala.collection.mutable._
import scala.io.Source
object TreeOfLife {
    sealed trait TreeNode {
        val data: Boolean
        protected def bool2String(v: Boolean): String = if (v) "X" else "."
    }
    case class Node(data: Boolean, left: TreeNode, right: TreeNode) extends TreeNode{
        override def toString(): String = "(%s %s %s)".format(left, bool2String(data), right)
    }
    case class Leaf(data: Boolean) extends TreeNode{
        override def toString(): String = bool2String(data)
    }

    type Key = (Boolean, Boolean, Boolean, Boolean)
    implicit def bool2int(v: Boolean): Int = if (v) 1 else 0
    implicit def char2Bool(c: Char): Boolean  = if(c == 'X') true else false
    implicit def bool2Char(b: Boolean): Char  = if(b) 'X' else '.'

    def deserialize(in: List[Char], acc: List[TreeNode]= List()):TreeNode = in match {
        case h::t if h == 'X' || h == '.' => deserialize(t, Leaf(h)::acc)
        case h::t if h == ')' =>  acc match {
            case r::p::l::accTail => deserialize(t, Node(p.data, l, r)::accTail)
            case _ => throw new Exception("Invalid")
        }
        case Nil => acc.head
        case _::t => deserialize(t, acc)
    }

    def populateRuleMap(ls: List[Key], n: Int) = {
        def populateMapInternal(remLs: List[Key] = ls,
                                remN: Int = n,
                                acc: Map[Key, Boolean] =Map()): Map[Key, Boolean] = remLs match {
            case Nil => acc
            case h::t => populateMapInternal(t, remN >> 1, acc updated (h, (remN & 1) == 1))
        }
        populateMapInternal()
    }

    def printResults(ruleMap: Map[Key, Boolean], stateStack: Stack[TreeNode], ls: List[(Int, String)]) = {
        def traversePath(t: TreeNode, path: List[Char]):Char = path.head match {
            case ']' => t.data
            case '>' => traversePath(t.asInstanceOf[Node].right, path.tail)
            case '<' => traversePath(t.asInstanceOf[Node].left, path.tail)
        }

        def modifyTree(t: TreeNode, one: Boolean = false):TreeNode = t match {
            case Node(d, l, r)  => val tup = (one, l.data, d, r.data)
                                   Node(ruleMap(tup), modifyTree(l, d), modifyTree(r, d))
            case Leaf(_) => Leaf(ruleMap((one, false, t.data, false)))
        }

        ls.foreach{x =>
            val step = x._1
            if (step < 0) {
               (1 to -step).foreach{_ =>
                    stateStack.pop
                }
            }

        if (step > 0){
           (1 to step).foreach{_ =>
               val newTree = modifyTree(stateStack.top)
               stateStack.push(newTree)
           }
        }
        println(traversePath(stateStack.top, x._2.toList.tail))
       }
    }

    def main(args: Array[String]) {

      val currentDirectory =  new java.io.File(".").getCanonicalPath
      val fileName = "test_treeOfLife.txt"
      val filePath = s"$currentDirectory/src/main/resources/$fileName"

      val lines = Source.fromFile(filePath).getLines()

        val (ruleNum, serializedTree, n) = (lines.next.toInt, lines.next, lines.next.toInt)
        val ls = for (_ <- 0 until n) yield {
            val ls  = lines.next.split(" ")
            (ls(0).toInt, ls(1))
        }

        val mapLs = (for (i<- 0 until 16) yield
                     ((i >> 3 & 1) == 1, (i >> 2 & 1) == 1 , (i >> 1 & 1) == 1, (i & 1) == 1)).toList

        val ruleMap = populateRuleMap(mapLs, ruleNum)
        val stateStack = Stack(deserialize(serializedTree.toList))
        printResults(ruleMap, stateStack, ls.toList)
    }
}
