import scala.util.Random

object MineSweeper{

  case class Tile(value: Char)

  case class MineSweeperBoard(r: Int, c:Int, numMines: Int){

    require(numMines <= r*c)
    val displayBoard = Array.fill[Char](r,c)('_')
    val boardArray = Array.fill[Tile](r,c)(Tile(' '))
    var mineCount = 0
    var unopenedTiles = r*c
    var gameLost = false

    def updateValue(x: Int, y:Int):Unit = {
     if (this.displayBoard(x)(y) != '_'){
       //if(!gameLost) println("You already stepped there my friend! Try other places")
     } else if (this.boardArray(x)(y) == Tile('X')){
       displayBoard(x)(y) = 'X'
       gameLost = true
     } else {
       val tilesAround = for(i<- x-1 to x+1 if i>=0 && i<r;
            j<- y-1 to y+1 if j>=0 && j<c;
            if (i,j)!=(x,y) && (this.displayBoard(i)(j)=='_' ||
            this.displayBoard(i)(j)=='X')) yield (i,j)

       if(!gameLost) {
          println(s"tile opened: ($x,$y)")
          println(s"tiles around: $tilesAround")
       }
       val values = tilesAround.map(k=> boardArray(k._1)(k._2).value)
       val res = values.count(_ == 'X')
       unopenedTiles -= 1
       displayBoard(x)(y) = res.toString.head
       res match {
         case 0 => //println(s"update tiles: $tilesAround")
                   tilesAround.foreach{k=> updateValue(k._1, k._2)}
         case _ => ()
       }
     }
    }

    def populateWithMines():Unit = mineCount match {
      case `numMines` => ()
      case _ => val rInt = Random.nextInt(r)
                 val cInt = Random.nextInt(c)
                 if (boardArray(rInt)(cInt) != Tile('X')) {
                   boardArray(rInt)(cInt) = Tile('X')
                   mineCount += 1
                 }
                 populateWithMines()
    }

    override def toString = {
      val res =
        if (gameLost) {
          for(i<- 0 until r;
              j<- 0 until r;
              if this.displayBoard(i)(j) == '_')
          this.updateValue(i,j)

          this.displayBoard.map{row=>
            row.toList.mkString(" ")
          }
        }
        else this.displayBoard.map{row=> row.toList.mkString(" ")}

      res.mkString("\n")
    }
  }

  def argumentsInBound(x:Int, y:Int, rows:Int, cols: Int) = {
      x>=0 && x<rows && y>=0 && y<cols
  }

  def listen(gameBoard: MineSweeperBoard):Unit = {
    println("""Enter (row,column)""")
    val in = scala.io.StdIn.readLine()
    val r_c = in.split(",").map(_.toInt)

    if(r_c.length !=2){
      println("two values needed atleast");listen(gameBoard)
    } else if (!argumentsInBound(r_c(0),r_c(1), gameBoard.r, gameBoard.c)){
      println("That's a coordinate outside the mine area...Come back in")
      listen(gameBoard)
    }else {
      gameBoard.updateValue(r_c(0),r_c(1))
      gameBoard.displayBoard(r_c(0))(r_c(1)) match {
        case 'X' => println("Oops! Stepped on a mine...Game over")
                    println(gameBoard)
        case _ if gameBoard.unopenedTiles == gameBoard.numMines => println(gameBoard); println("Victory! You are a minesweeper pro! B-)")
        case _ => println(gameBoard)
                  println("Good one...steady now");listen(gameBoard)
      }
    }
  }

  def main(args: Array[String]) = {

    println("Input board dimensions and number of mines in this format:(x,y,z)")
    val in = scala.io.StdIn.readLine()
    val r_c_n = in.split(",").map(_.toInt)
    val gameBoard = MineSweeperBoard(r_c_n(0),r_c_n(1),r_c_n(2))

    //populate board with Mine
    gameBoard populateWithMines()
    println("Get ready to sweep some mines!")
    println(gameBoard)
    listen(gameBoard)
  }
}
