import scala.collection.mutable._
import scala.io.Source
object SwapNodes {

  sealed trait TreeNode[+T]
  case class BranchNode[T](data: T,
    var left: TreeNode[T] = Empty,
    var right: TreeNode[T] = Empty) extends TreeNode[T] {
    override def toString(): String = "(%s %s %s)".format(left, data.toString, right)
  }
  case object Empty extends TreeNode[Nothing] {
    override def toString(): String = "-1"
  }

  implicit def int2Node(x: Int) = if (x == -1) Empty else BranchNode(x)

  def deserialize(lines: Iterator[String]) = {
    val root = BranchNode(1)
    val q: Queue[TreeNode[Int]] = Queue(root)

    def processTreeNode(t: TreeNode[Int]) = t match {
      case node: BranchNode[Int] =>
        val elems = lines.next.split(" ").map(_.toInt)
        node.left = elems(0)
        node.right = elems(1)
        q.enqueue(node.left, node.right)

      case _ => ()
    }

    def deserializeInternal(): Unit = q.size match {
      case 0 => ()
      case _ =>
        val curr = q.dequeue
        processTreeNode(curr)
        deserializeInternal()

    }

    val totalNodes = lines.next.toInt
    deserializeInternal()
    root
  }

  def swapNodes(root: BranchNode[Int], lines: Iterator[String]) = {
    val k = lines.next.toInt
    def swapNodesInternal(curr: TreeNode[Int], height: Int = 2): TreeNode[Int] = curr match {
      case Empty => Empty
      case node: BranchNode[Int] =>
        val (l, r) = if (height % k == 0) (node.right, node.left)
        else (node.left, node.right)
        BranchNode(node.data,
                   swapNodesInternal(l, height + 1),
                   swapNodesInternal(r, height + 1))
    }

    val res =
    if (k == 1) BranchNode(root.data, swapNodesInternal(root.right), swapNodesInternal(root.left))
    else BranchNode(root.data, swapNodesInternal(root.left), swapNodesInternal(root.right))

    println(inorder(res).mkString(" "))
    res
  }

  def process(n: Int, root: BranchNode[Int], lines: Iterator[String]):BranchNode[Int] = n match {
    case 0 => root
    case _ => process(n - 1, swapNodes(root, lines), lines)
  }

  def inorder(node: TreeNode[Int], acc: List[Int] = Nil):List[Int] = node match {
    case BranchNode(d, l, r) => val acc1 = inorder(l, acc)
                                val acc2 = acc1 :+ d
                                inorder(r, acc2)
    case _ => acc
  }

  def main(args: Array[String]) {

    val currentDirectory = new java.io.File(".").getCanonicalPath
    val fileName = "test_swapNodes.txt"
    val filePath = s"$currentDirectory/src/main/resources/$fileName"

    val lines = Source.fromFile(filePath).getLines()
    val tree = deserialize(lines)
    val n = lines.next.toInt
    process(n, tree, lines)
  }
}
