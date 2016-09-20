object BTreeSerialization{
  case class TreeNode(var data: Int, var left: Option[TreeNode] = None, var right: Option[TreeNode] = None)

  //pre-order
  def serialize(treeNode: TreeNode): List[Int] = {
    def serializeInternal(tNode: TreeNode, acc: List[Int] = Nil):List[Int] = tNode.data match {
      case -1 => acc :+ -1
      case _ =>  val p = acc :+ tNode.data
                 val l = serializeInternal(tNode.left.getOrElse(TreeNode(-1)), p)
                 serializeInternal(tNode.right.getOrElse(TreeNode(-1)), l)
    }
    serializeInternal(treeNode)
  }


  // def deserialize(ls: List[Int]): TreeNode = {
  //
  //   def deserializeInteral(rem: List[Int], acc: Option[TreeNode] = None): Option[TreeNode] = rem match {
  //     case Nil => acc
  //     case h::t if h == -1 =>  None
  //     case h::t =>
  //
  //   }
  //
  //   ls.length <3 match {
  //       case true => TreeNode(-1)
  //       case _ => deserializeInteral(ls).getOrElse(TreeNode(-1))
  //     }
  // }


  def main(args: Array[String]) = {
    val left = TreeNode(7, Some(TreeNode(3)), Some(TreeNode(8)))
    val right = TreeNode(20, Some(TreeNode(17)), Some(TreeNode(25)))
    val root = TreeNode(15, Some(left), Some(right))

    println(root)

    val serialized = serialize(root)
    println(s"Serialized Tree: -> $serialized")


    // val deserialized = deserialize(serialized)
    // println(s"Deserialized Tree: -> $deserialized")

  }
}
