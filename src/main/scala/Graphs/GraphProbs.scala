import scala.collection.mutable._

object GraphProbs{

  case class Graph(v: Int){
    var isVisited = Array.fill[Boolean](v)(false)

    var edges = Array.fill[Set[Int]](v)(Set())

    def addEdges(source: Int, destination: Int) = {
      edges(source) = edges(source) + destination
    }
    override def toString = {
      val vertices = (0 until v).map(_.toString).mkString(" ")
      val edgesString = (0 until edges.length).map(x => x.toString + " -> " +edges(x.toInt).toString).mkString("\n")
      s"""Vertices:
      $vertices
      Edges:
      $edgesString
      """
    }

    def visit(elem: Int) = {
      isVisited(elem) = true
    }

    def getUnvisitedChildren(elem: Int) = {
      val children = edges(elem)
      children.filterNot(x => isVisited(x))
    }

    def doBfs(start: Int) = {

      println("Breadth First Search:")
      var route: List[Int] = List()
      //intialize isVisited
      (0 until isVisited.length).foreach(x=> isVisited(x)= false)
      val q = Queue(start)
      while(q.nonEmpty){
        val elem = q.dequeue()
        visit(elem)
        route = route :+ elem
        getUnvisitedChildren(elem).foreach(q.enqueue(_))
      }
      println(s"$route\n")
    }

    def doDfs(start: Int) = {
      ???
    }

    def isCyclePresent() = {
      val state = false

      val vertexStack = Stack[Int]()
      (0 until (this.v)).foreach(vertexStack.push(_))

      while (vertexStack.nonEmpty && state == false){
        //intialize isVisited
        (0 until isVisited.length).foreach(x=> isVisited(x)= false)


      }
      // if(state) println("Cycle found!")
      // else println("No Cycle present")
    }

  }


  def main(args: Array[String]) = {

    //create graph of 5 nodes

    val numVertices = 6
    val g5 = Graph(numVertices)
    println(g5)
    //add edges
    g5.addEdges(0,1)
    g5.addEdges(1,0)
    g5.addEdges(0,2)
    g5.addEdges(2,1)
    g5.addEdges(2,3)
    g5.addEdges(2,4)
    g5.addEdges(4,5)

    //do bfs
    println(g5)

    //(0 until numVertices).foreach{
      //g5 doBfs(_)
      //g5 doDfs(_)
    //}
    g5 isCyclePresent()
  }
}
