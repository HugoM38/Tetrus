package ui
import logic.{Block, Grid}
import scalafx.scene.Scene
import logic.Grid.{blockList, cols, rows}
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle
import scalafx.scene.input.{KeyCode, KeyEvent}
import scalafx.Includes._
import scalafx.application.Platform

import java.util.Timer
import java.util.TimerTask
import scala.annotation.tailrec

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

  private def drawBlocks(): Unit = {
    for(block <- Grid.blockList) {
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
      drawBlocks()
  }

  private def startGame(): Unit = {
    val timer = new Timer()
    val task = new TimerTask {
      def run(): Unit = {
        Platform.runLater(() => {
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
    val rowsToRemove = (0 until rows).filter(row => (0 until cols).forall(col => blockList.contains(Block(col, row))))
    if (rowsToRemove.nonEmpty) {
      removeRow(rowsToRemove.head)
      checkAndRemoveRows()
    }
  }

  private def removeRow(rowToRemove: Int): Unit = {
      var newBlockList: Array[Block] = Array()
      newBlockList ++= blockList.filter(_.y < rowToRemove).map(block => Block(block.x, block.y + 1))
      newBlockList ++= blockList.filter(_.y > rowToRemove)
      blockList = newBlockList
      updateDisplay()
  }

  this.onKeyPressed = (event: KeyEvent) => {
    event.code match {
      case KeyCode.Left =>
        Grid.currentTetrominus = Grid.currentTetrominus.moveLeft
        updateDisplay()
        checkAndRemoveRows()
      case KeyCode.Right =>
        Grid.currentTetrominus = Grid.currentTetrominus.moveRight
        updateDisplay()
        checkAndRemoveRows()
      case KeyCode.Down =>
        Grid.currentTetrominus = Grid.currentTetrominus.moveDown
        updateDisplay()
        checkAndRemoveRows()
      case KeyCode.Up =>
        Grid.currentTetrominus = Grid.currentTetrominus.rotate
        updateDisplay()
        checkAndRemoveRows()
      case _ =>
    }
  }

  drawGrid()
  drawBlocks()
  startGame()

}
