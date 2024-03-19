package HexGame

import io.AnsiColor._
import scala.annotation.tailrec
import scala.util.Random
import TUI.printBoard

object Cells extends Enumeration with Serializable {
  type Cell = Value
  val Red = Value(s"${RED}X${RESET}")
  val Blue = Value(s"${BLUE}O${RESET}")
  val Empty = Value(s"${RESET}.")

}

case class MyRandom(seed: Long) extends Random {

  def nextMyInt(x: Int): (Int, MyRandom) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRandom = MyRandom(newSeed)
    val n = ((newSeed >>> 16).toInt) % x
    (if (n < 0) -n  else n, nextRandom)
  }
}

object HexUtils {

  type Board = List[List[Cells.Cell]] //Matrice of Cells
  val invalidMove = (-1, -1)
  val undo = (-2, -2)
  val startingLine = 0
  val startingCol = 0
  val emptyList = Nil

  // CreatingBoard
  def createCellsListCompact(cell: Cells.Cell, n: Int): List[Cells.Cell] = (1 to n map (_ => cell)).toList

  def createBoardCompact(n: Int): Board = (1 to n map (_ => createCellsListCompact(Cells.Empty, n))).toList

  def createPatternCompact(str: String, n: Int): String = (1 to n).foldLeft("": String)((result, _) => result + str)


  @tailrec // Encontrar os pontos por preencher no board
  def getLegMoves(board: Board, flst: List[(Int, Int)], lineN: Int): List[(Int, Int)] = {
    @tailrec
    def getLegMovesLine(line: List[Cells.Cell], lst: List[(Int, Int)], colN: Int): List[(Int, Int)] = {
      line match {
        case Nil => lst
        case x :: xs =>
          if (x == Cells.Empty) getLegMovesLine(xs, (lineN, colN) :: lst, colN + 1)
          else getLegMovesLine(xs, lst, colN + 1)
      }
    }

    board match {
      case Nil => flst
      case x :: xs => getLegMoves(xs, flst ++ getLegMovesLine(x, emptyList, startingCol), lineN + 1)
    }
  }

  def getLegalMoves(board: Board): List[(Int, Int)] = {
    def getLegalMoveLine(lst: List[Cells.Cell], lineN: Int): List[(Int, Int)] = lst.zipWithIndex.filter(_._1 == Cells.Empty).map(x => (lineN, x._2))

    board.zipWithIndex.foldLeft(List[(Int, Int)]())((x, y) => x ::: getLegalMoveLine(y._1, y._2))
  }

  //Dificuladade intermédia
  def playOnAdjacent(board: Board, player: Cells.Cell, random: MyRandom): ((Int, Int), MyRandom) = {
    def getAdjacentPos(row: Int, col: Int, player: Cells.Cell): List[(Int, Int)] = {
      player match {
        case Cells.Blue => List((row + 1, col), (row + 1, col - 1), (row, col + 1), (row, col - 1), (row - 1, col + 1), (row - 1, col))
        case _ => List((row, col + 1), (row - 1, col + 1), (row + 1, col), (row - 1, col), (row, col - 1), (row + 1, col - 1))
      }
    }

    val farestpoint = farestPoint(board, player)
    val startpoint = getStart(board, (0, 0), player)
    val validpoints = getAdjacentPos(farestpoint._1, farestpoint._2, player).filter((x) => (x._1 > -1 && x._2 > -1 && x._1 < board.size && x._2 < board.size && board(x._1)(x._2) == Cells.Empty))
    println(validpoints)
    if (validpoints.isEmpty) {
      if (startpoint == (-1, -1)) randomMove(board, random)
      else (startpoint, random)
    } else (validpoints(0), random)
  }

  def farestPoint(board: Board, player: Cells.Cell): (Int, Int) = {
    def farestPoint(rVis: Int, cVis: Int, maxRow: Int, maxCol: Int): (Int, Int) = {
      player match {
        case Cells.Red =>
          if (rVis == board.size) (maxRow, maxCol)
          else if (cVis == board.size) farestPoint(rVis + 1, 0, maxRow, maxCol)
          else if (board(rVis)(cVis) == player && cVis > maxCol) farestPoint(rVis, cVis + 1, rVis, cVis)
          else farestPoint(rVis, cVis + 1, maxRow, maxCol)

        case Cells.Blue =>
          if (cVis == board.size && player == Cells.Blue) (maxRow, maxCol)
          else if (rVis == board.size) farestPoint(0, cVis + 1, maxRow, maxCol)
          else if (board(rVis)(cVis) == player && rVis > maxRow) farestPoint(rVis + 1, cVis, rVis, cVis)
          else farestPoint(rVis + 1, cVis, maxRow, maxCol)
      }
    }

    farestPoint(0, 0, -1, -1)
  }

  def progression(row: Int, col: Int, player: Cells.Cell): (Int, Int) = player match {
    case Cells.Red => (row + 1, col)
    case Cells.Blue => (row, col + 1)
  }

  def getStart(board: Board, point: (Int, Int), player: Cells.Cell): (Int, Int) = {
    if (point._1 == board.size || point._2 == board.size) (-1, -1)
    board(point._1)(point._2) match {
      case Cells.Empty => point
      case _ => getStart(board, progression(point._1, point._2, player), player)
    }
  }


  def randomMove(board: Board, rand: MyRandom): ((Int, Int), MyRandom) = {
    val validMoves = getLegalMoves(board)
    if (validMoves.length != 0) {
      val (i1, r2) = rand.nextMyInt(validMoves.length)
      (validMoves(i1), r2)
    } else ((-1, -1), rand)
  }

  // StopGame
  def hasContiguousLine(board: Board): (Boolean, Cells.Cell)= {
    contLine(board, board.size)
  }

  def main(args: Array[String]): Unit = {
    printBoard(createBoardCompact(10))
  }

  def getFirstRedRows(board: Board): List[Int] = board.zipWithIndex.filter(x => x._1(0) == Cells.Red).map(_._2)

  def getFirstBlueCols(board: Board): List[Int] = board(0).zipWithIndex.filter(x => x._1 == Cells.Blue).map(_._2)

  val test = List(0,1,2,3)
  def contLine(board: Board , maxN : Int) :(Boolean,Cells.Cell) =  {

    def checkPoint(row : Int, col: Int, pointsVisited: List[(Int, Int)], player: Cells.Cell): Boolean = {
      if (row < 0 || col < 0 || row >= maxN || col >= maxN || pointsVisited.contains((row,col)) || board(row)(col) != player) false
      else if ((player == Cells.Red && col == maxN-1) || (player == Cells.Blue && row == maxN-1) ) true
      else {
        val lst = List((row + 1, col),(row - 1, col),(row, col + 1),(row, col - 1),(row - 1, col + 1),(row + 1, col - 1))
        lst.map( x => checkPoint(x._1,x._2, (row,col) :: pointsVisited,player )).foldLeft(false)(_||_)
      }
    }

    val blueCols = getFirstBlueCols(board)
    val redRows = getFirstRedRows(board)
    val blueWon = blueCols.map(x => checkPoint(0, x, Nil, Cells.Blue)).foldLeft(false)(_ || _)
    val redWon = redRows.map(x => checkPoint(x, 0, Nil, Cells.Red)).foldLeft(false)(_ || _)
    (redWon,blueWon) match {
      case (true,_) => (true,Cells.Red)
      case (_,true) => (true,Cells.Blue)
      case _ => (false,Cells.Empty)
    }
  }

}