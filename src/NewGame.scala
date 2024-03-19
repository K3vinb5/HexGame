import FxApp.{blueColor, cells, cellsColor, computerFirst, emptyColor, game, normalDiff, rand, redColor, setColor, startBoard, startGameState}
import HexGame.TUI.printBoard
import HexGame.{Cells, Game, GameState, MyRandom}
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.{Parent, Scene}
import javafx.scene.control.{Button, RadioButton}
import javafx.scene.shape.Polygon
import javafx.stage.{Modality, Stage}

class NewGame {

    @FXML
    var cancelbtn: Button = _

    @FXML
    var startNewGamebtn: Button = _

    @FXML
    var redColorrbtn: RadioButton = _

    @FXML
    var computerrbtn : RadioButton = _

    @FXML
    var normalDiffrbtn : RadioButton = _

    def cancel(): Unit = {
      // Fechar New Game
      val currentStage = cancelbtn.getScene.getWindow.asInstanceOf[Stage]
      currentStage.close()
      // Ir para o Menu
      val secondStage: Stage = new Stage()
      secondStage.initModality(Modality.APPLICATION_MODAL)
      val fxmlLoader = new FXMLLoader(getClass.getResource("Menu.fxml"))
      val mainViewRoot: Parent = fxmlLoader.load()
      val scene = new Scene(mainViewRoot)
      secondStage.setScene(scene)
      secondStage.show()
    }

  def startNewGame() : Unit = {
    // Fechar New Game
    val currentStage = startNewGamebtn.getScene.getWindow.asInstanceOf[Stage]
    currentStage.close()

    // Por as cores do jogo
    if (redColorrbtn.isSelected()) cells = (Cells.Red, Cells.Blue)
    else cells = (Cells.Blue, Cells.Red)
    // Por dificuldade
    if (normalDiffrbtn.isSelected()) normalDiff = true

    val startSeed = new scala.util.Random
    val rand = MyRandom(startSeed.nextLong())
    startGameState = GameState(startBoard, cells, rand)
    game =  Game(List(startGameState), cells, computerFirst, normalDiff)
    setColor()

    // Ir para o Menu
    val secondStage: Stage = new Stage()
    secondStage.initModality(Modality.APPLICATION_MODAL)
    val fxmlLoader = new FXMLLoader(getClass.getResource("Game.fxml"))
    val mainViewRoot: Parent = fxmlLoader.load()
    val scene = new Scene(mainViewRoot)
    secondStage.setScene(scene)
    secondStage.show()
    if (computerrbtn.isSelected()){
      computerFirst = true
      val (newBoard, newRandom, botMove) = game.gs.head.playIA2()
      val poly1 = scene.lookup("#id" + botMove._1 + "" + botMove._2).asInstanceOf[Polygon]
      poly1.setFill(cellsColor._2)
      game = game.addBoard(newBoard, newRandom)
      scene.lookup("#undobtn").asInstanceOf[Button].setDisable(false)
    }
  }
}
