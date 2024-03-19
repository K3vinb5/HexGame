package HexGame

import HexGame.HexUtils.{Board, createPatternCompact, hasContiguousLine, invalidMove, undo}
import scala.annotation.tailrec
import scala.io.AnsiColor.{BLUE, RED, RESET}
import scala.io.StdIn.readLine

object TUI {
  val space: String = " "
  val pointSymbol: String = "*"
  val pointSeparator: String = "   "
  val playableSeparator: String = " - "

  // Show Board
  def printFirstBlueLine(n: Int): Any = println(space + createPatternCompact(s"${BLUE}$pointSymbol${RESET}" + pointSeparator, n))

  def printLastBlueLine(n: Int): Any = println(createPatternCompact(space, (n - 1) * 2 + 3) + createPatternCompact(s"${BLUE}$pointSymbol${RESET}" + pointSeparator, n))

  def printBoard(board: Board): Any = {
    val backSlash = "\\"
    val slash = "/ "
    val redCell = s"${RED}$pointSymbol${RESET}"
    val boardLen = board.length

    @tailrec
    def printBoard(board: Board, n: Int): Any = {
      board match {
        case List(h) => printCellsCompact(h, playableSeparator, createPatternCompact(space, n) + redCell + space, space + redCell + "\n")
        case h :: tail => {
          printCellsCompact(h, playableSeparator, createPatternCompact(space, n) + redCell + space, space + redCell + "\n")
          print(createPatternCompact(space, n + 3) + createPatternCompact(backSlash + space + slash, boardLen - 1) + backSlash + "\n")
          printBoard(tail, n + 2)
        }
      }
    }

    printFirstBlueLine(boardLen)
    printBoard(board, 0)
    printLastBlueLine(boardLen)
  }

  def printGameOver(): Unit = println(s"\n=== GAME OVER ===")

  def printGameWon(board: Board): (Boolean) = { //TUI
    val (gameEnded, winner) = hasContiguousLine(board)
    if (gameEnded) {
      printGameOver()
      val blueWon = winner == Cells.Blue
      if (blueWon) println("BLUE WON THE GAME")
      else println("RED WON THE GAME")
    }
    gameEnded
  }


  def printCellsCompact(line: List[Cells.Cell], separator: String, first: String, last: String): Any = {
    print(first)
    (line take line.length - 1) map (x => print(x + separator))
    print(line.last + last)
  }

  // User Actions
  def getUserInput(): String = readLine.trim.toUpperCase

  def showExampleDecision(): Unit = print("\nFormat for a position: [Row] [Column] or enter (U)ndo your previous move")

  def showPrompt(): Unit = print("\nEnter a position to play: ")

  def asksBoardSize(): String = {
    print("\nEnter the board size: ")
    getUserInput()
  }

  def asksPlayerColor(): (Cells.Cell, Cells.Cell) = {
    print("\nChoose a color to play\nEnter (R)ed or (B)lue: ")
    getUserInput() match {
      case "R" => (Cells.Red, Cells.Blue)
      case "B" => (Cells.Blue, Cells.Red)
      case _ => {
        print("\nInvalid Color, assigned Red.")
        (Cells.Red, Cells.Blue)
      }
    }
  }


  def playerDecision(str: String): (Int, Int) = {
    if (str == "EXIT") System.exit(0)
    if (str == "U") return undo
    str.split(" ").toList match {
      case List(x, y) =>
        if ((x forall Character.isDigit) && (y forall Character.isDigit)) (x.toInt, y.toInt)
        else invalidMove
      case _ => invalidMove
    }
  }
  def askAndPlay(gameState: GameState, row: Int, col: Int): Board = {


    @tailrec
    def keepAsking(board: Board, bool: Boolean): Board = {
      if (bool) {
        board
      } else {
        print("\nInvalid Position!\n")
        showPrompt()
        val decision = playerDecision(getUserInput())
        val newBoard = gameState.play( decision._1, decision._2)
        keepAsking(newBoard._1, newBoard._2)
      }
    }

    val newBoard = gameState.play(row, col)

    keepAsking(newBoard._1, newBoard._2)

  }


}
