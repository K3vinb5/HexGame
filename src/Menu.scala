import FxApp.{computerFirst, doWithFilter, filterDo, game, getCells, normalDiff, setColor}
import HexGame.Game
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.{Parent, Scene}
import javafx.scene.control.{Button, TextField}
import javafx.stage.{Modality, Stage}
import javafx.scene.control.ChoiceBox


class Menu {

  @FXML
  var newGamebtn: Button = _

  @FXML
  var saveschoicebox : ChoiceBox[String] = _

  def quit(): Unit = {
    System.exit(0)
  }

  def newGame() : Unit = {
    // Fechar Menu
    val currentStage = newGamebtn.getScene.getWindow.asInstanceOf[Stage]
    currentStage.close()
    // Abrir New Game
    val secondStage: Stage = new Stage()
    secondStage.initModality(Modality.APPLICATION_MODAL)
    val fxmlLoader = new FXMLLoader(getClass.getResource("NewGame.fxml"))
    val mainViewRoot: Parent = fxmlLoader.load()
    val scene = new Scene(mainViewRoot)
    secondStage.setScene(scene)
    secondStage.show()
  }

  def load(): Unit = {
    if (saveschoicebox.getValue != null)  {
      // Fechar Menu
      val currentStage = newGamebtn.getScene.getWindow.asInstanceOf[Stage]
      currentStage.close()
      // Abrir New Game
      val secondStage: Stage = new Stage()
      secondStage.initModality(Modality.APPLICATION_MODAL)
      val fxmlLoader = new FXMLLoader(getClass.getResource("Game.fxml"))
      val mainViewRoot: Parent = fxmlLoader.load()
      val scene = new Scene(mainViewRoot)
      secondStage.setScene(scene)
      secondStage.show()
      game = Game.readFromFile(saveschoicebox.getValue)
      game = Game(game.gs, game.gs.head.cells, computerFirst, normalDiff)
      doWithFilter(getCells(game.gs.head.board), filterDo, scene)
      if (game.gs.size != 1) scene.lookup("#undobtn").asInstanceOf[Button].setDisable(false)
      scene.lookup("#fileName").asInstanceOf[TextField].setText(saveschoicebox.getValue)
      setColor()
    }
  }

}