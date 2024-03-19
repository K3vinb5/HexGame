import FxApp.{blueColor, cells, cellsColor, computerFirst, doWithFilter, emptyColor, filterDo, game, getCells, normalDiff, redColor, startGameState}
import HexGame.HexUtils.Board
import HexGame.TUI.printBoard
import HexGame.{Cells, GameState}
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.{Parent, Scene}
import javafx.scene.control.{Button, ChoiceBox, Control, Label, TextField}
import javafx.scene.input.MouseEvent
import javafx.scene.paint.Color
import javafx.scene.shape.Polygon
import javafx.stage.{Modality, Stage}

import java.nio.file.{FileSystems, Files}
import scala.::
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.{CollectionHasAsScala, IteratorHasAsScala}

class Game {
    @FXML
    var goToMenubtn: Button = _

    @FXML
    var fileName : TextField = _

    @FXML
    var finalMessage: Label = _

    @FXML
    var undobtn : Button = _

    def goToMenu(): Unit = {
        // Fechar Game
        val currentStage = goToMenubtn.getScene.getWindow.asInstanceOf[Stage]
        currentStage.close()
        // Ir para o Menu
        val secondStage: Stage = new Stage()
        secondStage.initModality(Modality.APPLICATION_MODAL)
        val fxmlLoader = new FXMLLoader(getClass.getResource("Menu.fxml"))
        val mainViewRoot: Parent = fxmlLoader.load()
        val scene = new Scene(mainViewRoot)
        secondStage.setScene(scene)
        secondStage.show()
        val dir = FileSystems.getDefault.getPath("Saves")
        val savesList = Files.list(dir).iterator().asScala.toList.map(x => x.toString.substring(6))
        val cb = scene.lookup("#saveschoicebox").asInstanceOf[ChoiceBox[String]]
        savesList.map(x => cb.getItems.add(x))
    }

    def play(mouseEvent: MouseEvent) : Unit = {

      // Buscar Hexagono
        val poly = mouseEvent.getSource.asInstanceOf[Polygon]
      // Buscar identificador do Hexagono
        val polyID = poly.getId.substring(2)
        val move = (polyID.charAt(0).toInt - '0',polyID.charAt(1).toInt - '0')
      // Atualizar o board
        val (board, success) = game.gs.head.play(move._1, move._2)

      //printBoard(board)
        if (success) {
          undobtn.setDisable(false)
          // Pintar Hexagono
          poly.setFill(cellsColor._1)
          val playerState = GameState(board, cells, game.gs.head.random)
          val temp = game.addBoard(playerState.board, playerState.random)
          // Computer's Turn
          val (newBoard, newRandom, botMove) = temp.playIADiff()
          if (botMove._1 != -1 && botMove._2 != -1) {
            val poly1 = goToMenubtn.getScene.lookup("#id" + botMove._1 + "" + botMove._2).asInstanceOf[Polygon]
            poly1.setFill(cellsColor._2)
            game = game.addBoard(newBoard, newRandom)
            //printBoard(newBoard)
          } else {
            gameEnded(cells._1)
          }
        }
      val (hasEnded, winner) = game.gs.head.hasContinuosLine()
      if (hasEnded) gameEnded(winner)
    }

    def save() = {
        if (fileName.getText != "") game.writeToFile(fileName.getText)
    }

    def undo() = {
      game = game.undo()
      doWithFilter(getCells(game.gs.head.board), filterDo, goToMenubtn.getScene)
      if (game.gs.size == 1) {
        undobtn.setDisable(true)
        if (computerFirst){
          val (newBoard, newRandom, botMove) = game.playIADiff()
          val polyComp = goToMenubtn.getScene.lookup("#id" + botMove._1 + "" + botMove._2).asInstanceOf[Polygon]
          polyComp.setFill(cellsColor._2)
          game = game.addBoard(newBoard, newRandom)
          undobtn.setDisable(false)
        }
      }
    }

    def gameEnded(winner: Cells.Cell) = {
      // Mostrar Mensagem
      if (winner.equals(game.gs.head.cells._1)) {
        finalMessage.setText("YOU WON!!!")
        finalMessage.setTextFill(cellsColor._1)
      } else {
        finalMessage.setText("YOU LOST :(")
        finalMessage.setTextFill(cellsColor._2)
      }
      goToMenubtn.getScene.lookup(" ").lookupAll(".button").asScala.toList.map(x=>x.setVisible(false))
      goToMenubtn.getScene.lookup(" ").lookupAll(".label").asScala.toList.map(x=>x.setVisible(false))
      goToMenubtn.getScene.lookup(" ").lookupAll(".textfield").asScala.toList.map(x=>x.setVisible(false))
      goToMenubtn.getScene.lookup(" ").lookupAll(".polygon").asScala.toList.map(x=>x.setMouseTransparent(true))
      goToMenubtn.getScene.lookup("#goToMenubtn").asInstanceOf[Button].setVisible(true)
      finalMessage.setVisible(true)
    }

}
