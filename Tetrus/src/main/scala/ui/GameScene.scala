package ui

import logic.{Block, Grid}
import scalafx.scene.Scene
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Line, Rectangle}
import scalafx.scene.input.{KeyCode, KeyEvent}
import scalafx.Includes._
import scalafx.application.Platform
import scalafx.scene.control.Button
import scalafx.scene.text.Text

import java.util.Timer
import java.util.TimerTask
import scala.annotation.tailrec

class GameScene extends Scene {

  private val tetrusPane = new Pane()
  private val cellSize = 20
  private var gameStarted = false
  private var timer = new Timer()

  private def drawGrid(): Unit = {
    for (i <- 0 until Grid.rows; j <- 0 until Grid.cols) {
      val rect = new Rectangle {
        x = j * cellSize
        y = i * cellSize
        width = cellSize
        height = cellSize
        fill = Color.Black
        stroke = Color.Green
      }

      val dottedLine = new Line {
        startX = 0
        endX = Grid.cols * cellSize
        startY = 5 * cellSize
        endY = 5 * cellSize
        stroke = Color.Yellow
        strokeWidth = 2
        strokeDashArray.addAll(10.0, 5.0)
      }
      tetrusPane.children.addAll(rect, dottedLine)
    }
  }

  content = List(tetrusPane)

  private def drawBlocks(): Unit = {
    for(block <- Grid.blockList) {
      val rect = new Rectangle{
        x = block.x * cellSize
        y = block.y * cellSize
        width = cellSize
        height = cellSize
        fill = Color.Blue
        stroke = Color.White
      }
      tetrusPane.children.add(rect)
    }
  }

  private def updateDisplay(): Unit = {
    if (!gameStarted) return
    tetrusPane.children.clear()
    drawGrid()
    drawBlocks()
  }

  private def startGame(): Unit = {
    gameStarted = true
    timer = new Timer()
    val task = new TimerTask {
      def run(): Unit = {
        Platform.runLater(() => {
          checkIfLost()
          Grid.currentTetrominus = Grid.currentTetrominus.moveDown
          updateDisplay()
          checkAndRemoveRows()
        })
      }
    }
    timer.scheduleAtFixedRate(task, 0, 1000)
  }

  @tailrec
  private def checkAndRemoveRows(): Unit = {
    if(Grid.currentTetrominus.canMoveDown) return
    val rowsToRemove = (0 until Grid.rows).filter(row => (0 until Grid.cols).forall(col => Grid.blockList.contains(Block(col, row))))
    if (rowsToRemove.nonEmpty) {
      removeRow(rowsToRemove.head)
      checkAndRemoveRows()
    }
  }

  private def removeRow(rowToRemove: Int): Unit = {
    val newBlockList = Grid.blockList.filter(_.y < rowToRemove).map(block => Block(block.x, block.y + 1)) ++
      Grid.blockList.filter(_.y > rowToRemove)
    Grid.blockList = newBlockList
    updateDisplay()
  }

  private def checkIfLost(): Unit = {
    if(Grid.blockList.exists(_.y == 4) && !Grid.currentTetrominus.canMoveDown) {
      gameStarted = false
      val gameOverBackground = new Rectangle {
        x = 0
        y = 0
        width = Grid.cols * cellSize
        height = Grid.rows * cellSize
        fill = Color.Black
      }

      val gameOverText = new Text {
        text = "Game Over"
        style = "-fx-font: 24 arial;"
        fill = Color.White
      }

      val restartButton = new Button("Restart") {
        layoutX = (Grid.cols * cellSize) / 2 - 50
        layoutY = (Grid.rows * cellSize) / 2 + 30
        onAction = _ => restartGame()
      }

      tetrusPane.children.addAll(gameOverBackground, gameOverText, restartButton)

      gameOverText.x = (Grid.cols * cellSize - gameOverText.boundsInLocal.get().getWidth) / 3
      gameOverText.y = (Grid.rows * cellSize) / 2

      timer.cancel()
    }
  }

  private def restartGame(): Unit = {
    Grid.blockList = Array()
    Grid.currentTetrominus = Grid.nextTetrominus
    startGame()
  }

  this.onKeyPressed = (event: KeyEvent) => {
    event.code match {
      case KeyCode.Left =>
        if (gameStarted) {
          Grid.currentTetrominus = Grid.currentTetrominus.moveLeft
          checkIfLost()
          updateDisplay()
          checkAndRemoveRows()
        }
      case KeyCode.Right =>
        if (gameStarted) {
          Grid.currentTetrominus = Grid.currentTetrominus.moveRight
          checkIfLost()
          updateDisplay()
          checkAndRemoveRows()
        }
      case KeyCode.Down =>
        if (gameStarted) {
          Grid.currentTetrominus = Grid.currentTetrominus.moveDown
          checkIfLost()
          updateDisplay()
          checkAndRemoveRows()
        }
      case KeyCode.Up =>
        if (gameStarted) {
          Grid.currentTetrominus = Grid.currentTetrominus.rotate
          checkIfLost()
          updateDisplay()
          checkAndRemoveRows()
        }
      case KeyCode.Space =>
        if (gameStarted) {
          Grid.currentTetrominus = Grid.currentTetrominus.fullMoveDown
          checkIfLost()
          updateDisplay()
          checkAndRemoveRows()
        }
      case _ =>
    }
  }

  drawGrid()
  drawBlocks()
  startGame()
}
