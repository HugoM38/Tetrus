import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.scene.layout.StackPane

object Main extends JFXApp3 {
  override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      title = "Tetrus"
      scene = new Scene(300, 200) {
        root = new StackPane {
          children = new Button("Click Me")
        }
      }
    }
  }
}