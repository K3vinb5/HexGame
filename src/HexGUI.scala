import HexGame.HexUtils.{Board, createBoardCompact}
import HexGame.TUI.printBoard
import HexGame.{Cells, Game, GameState, MyRandom}
import com.sun.javafx.scene.control.behavior.ChoiceBoxBehavior
import javafx.application.Application
import javafx.fxml.FXMLLoader
import javafx.scene.paint.Color
import javafx.scene.{Parent, Scene}
import javafx.scene.shape.Polygon
import javafx.stage.Stage
import javafx.scene.control.ChoiceBox

import java.nio.file.{FileSystems, Files}
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.reflect.internal.util.Collections

class HexGUI extends Application {

  override def start(primaryStage: Stage): Unit = {
    primaryStage.setTitle("Hex Game App")
    val fxmlLoader = new FXMLLoader(getClass.getResource("Menu.fxml"))
    val scene = new Scene(fxmlLoader.load())
    primaryStage.setScene(scene)
    val dir = FileSystems.getDefault.getPath("Saves")
    val savesList = Files.list(dir).iterator().asScala.toList.map(x=>x.toString.substring(6))
    val cb = scene.lookup("#saveschoicebox").asInstanceOf[ChoiceBox[String]]
    savesList.map(x => cb.getItems.add(x))
    primaryStage.show()
  }
}
  object FxApp {
    val emptyColor: Color = Color.LIGHTGREY
    val redColor: Color = Color.web("#FF2424")
    val blueColor: Color = Color.web("#3168FF")

    val startBoard = createBoardCompact(5)
    val startSeed = new scala.util.Random
    val rand = MyRandom(startSeed.nextLong())

    var computerFirst = false
    var normalDiff = false
    var cells = (Cells.Empty, Cells.Empty)
    var startGameState = GameState(startBoard, cells, rand)
    var game = Game(List(startGameState), cells, computerFirst, normalDiff)
    var cellsColor : (Color, Color) = (emptyColor, emptyColor)


    def filterDo(cellModified: (Cells.Cell, Int, Int), scene : Scene) = {
      cellModified._1 match {
        case Cells.Blue => scene.lookup("#id" + cellModified._2 + "" + cellModified._3).asInstanceOf[Polygon].setFill(blueColor)
        case Cells.Red => scene.lookup("#id" + cellModified._2 + "" + cellModified._3).asInstanceOf[Polygon].setFill(redColor)
        case _ => scene.lookup("#id" + cellModified._2 + "" + cellModified._3).asInstanceOf[Polygon].setFill(emptyColor)
      }
    }

    @tailrec
    def doWithFilter[A](elements: List[A], myFunc: (A, Scene) => Unit, scene : Scene): Unit = {
      elements match {
        case Nil => Nil
        case h :: tail => {
          myFunc(h,scene)
          doWithFilter(tail, myFunc, scene)
        }
      }
    }

    def getCells(board: Board): List[(Cells.Cell, Int, Int)] = {
      def getCellsLine(lst: List[Cells.Cell], lineN: Int): List[(Cells.Cell, Int, Int)] = lst.zipWithIndex.map(x => (x._1, lineN, x._2))

      board.zipWithIndex.foldLeft(List[(Cells.Cell, Int, Int)]())((x, y) => x ::: getCellsLine(y._1, y._2))
    }

    def setColor() = {
      if (game.gs.head.cells._1.equals(Cells.Red)) cellsColor = (redColor, blueColor)
      else cellsColor = (blueColor, redColor)
    }

    def main(args: Array[String]): Unit = {
      Application.launch(classOf[HexGUI], args: _*)
    }
  }


