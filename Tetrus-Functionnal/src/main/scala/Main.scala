import scalafx.application.JFXApp3
import ui.GameScene

object Main extends JFXApp3 {
  override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      title = "Tetrus"
      scene = new GameScene
    }
  }
}