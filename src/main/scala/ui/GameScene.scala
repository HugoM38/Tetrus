package ui
import logic.Grid
import scalafx.scene.Scene
import logic.Grid.rows
import logic.Grid.cols
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle
import scalafx.scene.input.{KeyCode, KeyEvent}
import scalafx.Includes._
import scalafx.application.Platform

import java.util.Timer
import java.util.TimerTask

class GameScene extends Scene {

  private val tetrusPane = new Pane()

  private val cellSize = 20

  private def drawGrid(): Unit = {
    for (i <- 0 until rows; j <- 0 until cols) {
      val rect = new Rectangle {
        x = j * cellSize
        y = i * cellSize
        width = cellSize
        height = cellSize
        fill = Color.Black
        stroke = Color.Green
      }
      tetrusPane.children.add(rect)
    }
  }

  content = List(tetrusPane)

  private def drawPiece(): Unit = {
    for(block <- Grid.currentTetrominus.blocks) {
      val rect = new Rectangle{
        x = block.x * cellSize
        y = block.y * cellSize
        width = cellSize
        height = cellSize
        fill = Color.Blue
      }
      tetrusPane.children.add(rect)
    }
  }

  private def updateDisplay(): Unit = {
      tetrusPane.children.clear()
      drawGrid()
      drawPiece()
  }

  private def startGame(): Unit = {
    val timer = new Timer()
    val task = new TimerTask {
      def run(): Unit = {
        Platform.runLater(() => {
          Grid.currentTetrominus = Grid.currentTetrominus.moveDown
          updateDisplay()
        })
      }
    }
    timer.scheduleAtFixedRate(task, 0, 1000)
  }

  this.onKeyPressed = (event: KeyEvent) => {
    event.code match {
      case KeyCode.Left => Grid.currentTetrominus = Grid.currentTetrominus.moveLeft
      case KeyCode.Right => Grid.currentTetrominus = Grid.currentTetrominus.moveRight
      case KeyCode.Down => Grid.currentTetrominus = Grid.currentTetrominus.moveDown
      case KeyCode.Up => Grid.currentTetrominus = Grid.currentTetrominus.rotate
      case _ =>
    }
    updateDisplay()
  }

  drawGrid()
  drawPiece()
  startGame()

}
