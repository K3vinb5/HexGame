package HexGame

import HexGame.HexUtils.{Board, contLine, createBoardCompact, hasContiguousLine, playOnAdjacent, randomMove, undo}
import HexGame.TUI.{askAndPlay, asksBoardSize, asksPlayerColor, getUserInput, playerDecision, printBoard, printGameOver, printGameWon, showExampleDecision, showPrompt}

import scala.annotation.tailrec
import java.io._

case class GameState(board: Board, cells: (Cells.Cell,Cells.Cell), random: MyRandom) extends Serializable {
  def play(row: Int, col: Int): (Board, Boolean) = GameState.play(this.board, this.cells._1, row, col)

  def playIA(): (Board, MyRandom, (Int, Int)) = GameState.playIA(this.board, this.cells._2, this.random)
  def playIA2(): (Board, MyRandom, (Int, Int)) = GameState.playIA2(this.board, this.cells._2, this.random)

  def hasContinuosLine(): (Boolean, Cells.Cell) = GameState.hasContinuosLine(this.board)
}

object GameState {
  def play(board: Board, player: Cells.Cell, row: Int, col: Int): (Board, Boolean) = {

    val validPosition = (row >= 0 && col >= 0 && row < board.size && col < board.size && board(row)(col) == Cells.Empty)

    if (validPosition) {
      val newBoardWithUpdatedRow = board(row).updated(col, player) //updates row on given column
      val newBoardFullyUploaded = board.updated(row, newBoardWithUpdatedRow) //updates column
      (newBoardFullyUploaded, validPosition) //returns updated board
    } else {
      (board, validPosition) //returns the same board
    }
  }

  def playIA(board: Board, player: Cells.Cell, rnd: MyRandom): (Board, MyRandom, (Int,Int)) = {
    val (rndMove, nextRandom) = playOnAdjacent(board, player, rnd) //randomMove(board,rnd)
    val (newBoard, _) = play(board, player, rndMove._1, rndMove._2)
    (newBoard, nextRandom, rndMove)
  }

  def playIA2(board: Board, player: Cells.Cell, rnd: MyRandom): (Board, MyRandom, (Int, Int)) = {
    val (rndMove, nextRandom) = randomMove(board, rnd)
    val (newBoard, _) = play(board, player, rndMove._1, rndMove._2)
    (newBoard, nextRandom, rndMove)
  }

  def hasContinuosLine(board: Board): (Boolean, Cells.Cell) = {
    contLine(board, board.size)
  }

}

case class Game(gs : List[GameState],cells: (Cells.Cell,Cells.Cell), first : Boolean, difficultynormal : Boolean) extends Serializable {

  def undo() : Game = Game.undo(this)

  def addBoard(newBoard: Board , newRandom : MyRandom) : Game = Game.addBoard(newBoard,newRandom,this)

  def writeToFile(filenameOut: String) = Game.writeToFile(filenameOut, this)

  def playIADiff() = Game.playIADiff(this)

}

object Game {


  def undo(game : Game) : Game = {
    game.gs match {
      case List(head) => game
      case head :: lst => Game(lst,game.cells, game.first, game.difficultynormal)
    }
  }

  def addBoard( newBoard: Board, newRandom : MyRandom , game: Game) : Game =  {
    val newGameState = GameState(newBoard, game.cells,newRandom)
    Game( newGameState:: game.gs, game.cells, game.first, game.difficultynormal)
  }

  def writeToFile(name: String, game : Game) {
    var outObj = None: Option[ObjectOutputStream]
    var out = None: Option[FileOutputStream]
    try {
      out = Some(new FileOutputStream("Saves//" + name))
      outObj = Some(new ObjectOutputStream(out.get))
      outObj.get.writeObject(game)
    } catch {
      case e: IOException => e.printStackTrace
    }
  }

  def readFromFile(name: String) : Game = {
    var inObj = None: Option[ObjectInputStream]
    var in = None: Option[FileInputStream]
    try {
      in = Some(new FileInputStream("Saves//" + name))
      inObj = Some(new ObjectInputStream(in.get))
    } catch {
      case e: IOException => e.printStackTrace
    }
    inObj.get.readObject().asInstanceOf[Game]
  }

  def playIADiff(game: Game): (Board, MyRandom, (Int, Int)) = {
    if (game.difficultynormal) game.gs.head.playIA()
    else game.gs.head.playIA2()
  }
}


object Hex extends App {


  val startBoard = createBoardCompact(asksBoardSize().toInt)
  val rand = MyRandom(11)
  val cells = asksPlayerColor()

  val startGameState = GameState(startBoard,cells,rand)
  val game = Game(List(startGameState),cells, false, false)

  showExampleDecision()

  mainLoop(game)

  @tailrec
  def mainLoop(game: Game): Unit = {

    showPrompt()

    val decision = playerDecision(getUserInput())

    if (decision != undo) {
      // Player's Turn
      val playerBoard = askAndPlay(game.gs.head, decision._1, decision._2)
      val playerState = GameState(playerBoard,game.cells,game.gs.head.random)
      // Computer's Turn
      val (newBoard, newRandom, _) = playerState.playIA()

      val newGame = game.addBoard(newBoard, newRandom)
      printBoard(newBoard)

      // Next Turn or Game Over
      val (gameWon) = printGameWon(newBoard)
      if (!gameWon) mainLoop(newGame)
    }
    // Return to previous Board
    else {
      mainLoop(game.undo())
    }
  }
}
