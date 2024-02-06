package ui

import logic.{Block, Grid, Tetrominus}
import scalafx.scene.Scene
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Line, Rectangle}
import scalafx.Includes._
import scalafx.scene.input.{KeyCode, KeyEvent}
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

  private def drawBlocks(blockList: List[Block]): Unit = {
    for(block <- blockList) {
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

  private def updateDisplay(blockList: List[Block]): List[Block] = {
    if (!gameStarted) return blockList
    tetrusPane.children.clear()
    drawGrid()
    drawBlocks(blockList)
    blockList
  }

  private def startGame(): Unit = {
    gameStarted = true
    timer = new Timer()
    val task = new TimerTask {
      def run(): Unit = {
        Platform.runLater(() => {
          val (newBlockList , newTetrominus) = checkIfLost(Grid.currentBlockList)
          Grid.currentBlockList = newBlockList
          Grid.currentTetrominus = newTetrominus
          val (newBlockList2 , newTetrominus2) = Grid.currentTetrominus.moveDown(Grid.currentBlockList)
          Grid.currentBlockList = newBlockList2
          Grid.currentTetrominus = newTetrominus2
          Grid.currentBlockList = updateDisplay(Grid.currentBlockList)
          Grid.currentBlockList = checkAndRemoveRows(Grid.currentBlockList)
        })
      }
    }
    timer.scheduleAtFixedRate(task, 0, 1000)
  }

  @tailrec
  private def checkAndRemoveRows(blockList: List[Block]): List[Block] = {
    if(Grid.currentTetrominus.canMoveDown(blockList)) return blockList
    val rowsToRemove = (0 until Grid.rows).filter(row => (0 until Grid.cols).forall(col => blockList.contains(Block(col, row))))
    if (rowsToRemove.nonEmpty) {
      checkAndRemoveRows(removeRow(blockList ,rowsToRemove.head))
    } else {
      blockList
    }
  }

  private def removeRow(blockList: List[Block], rowToRemove: Int): List[Block] = {
    val newBlockList = blockList.filter(_.y < rowToRemove).map(block => Block(block.x, block.y + 1)) ++
      blockList.filter(_.y > rowToRemove)
    updateDisplay(newBlockList)
  }

  private def checkIfLost(blockList: List[Block]): (List[Block], Tetrominus) = {
    if(blockList.exists(_.y == 4) && !Grid.currentTetrominus.canMoveDown(blockList)) {
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

      var newBlockList: List[Block] = List()
      var newTetrominus: Tetrominus = Grid.currentTetrominus

      val restartButton = new Button("Restart") {
        layoutX = (Grid.cols * cellSize) / 2 - 50
        layoutY = (Grid.rows * cellSize) / 2 + 30
        onAction = _ => {
          val (newBlockList2, newTetrominus2 ) = restartGame()
          newBlockList = newBlockList2
          newTetrominus = newTetrominus2
        }
      }

      tetrusPane.children.addAll(gameOverBackground, gameOverText, restartButton)

      gameOverText.x = (Grid.cols * cellSize - gameOverText.boundsInLocal.get().getWidth) / 3
      gameOverText.y = (Grid.rows * cellSize) / 2

      timer.cancel()
      (newBlockList, newTetrominus)
    } else {
      (blockList, Grid.currentTetrominus)
    }
  }

  private def restartGame(): (List[Block], Tetrominus) = {
    val (blockList, newTetrominus) = Grid.nextTetrominus(List())
    startGame()
    (blockList, newTetrominus)
  }

  this.onKeyPressed = (event: KeyEvent) => {
    event.code match {
      case KeyCode.Left =>
        if (gameStarted) {
          val (newBlockList, newTetrominus) = Grid.currentTetrominus.moveLeft(Grid.currentBlockList)
          Grid.currentBlockList = newBlockList
          Grid.currentTetrominus = newTetrominus
          val (newBlockList2, newTetrominus2) = checkIfLost(Grid.currentBlockList)
          Grid.currentBlockList = newBlockList2
          Grid.currentTetrominus = newTetrominus2
          Grid.currentBlockList = updateDisplay(Grid.currentBlockList)
          Grid.currentBlockList = checkAndRemoveRows(Grid.currentBlockList)
        }
      case KeyCode.Right =>
        if (gameStarted) {
          val (newBlockList, newTetrominus) = Grid.currentTetrominus.moveRight(Grid.currentBlockList)
          Grid.currentBlockList = newBlockList
          Grid.currentTetrominus = newTetrominus
          val (newBlockList2, newTetrominus2) = checkIfLost(Grid.currentBlockList)
          Grid.currentBlockList = newBlockList2
          Grid.currentTetrominus = newTetrominus2
          Grid.currentBlockList = updateDisplay(Grid.currentBlockList)
          Grid.currentBlockList = checkAndRemoveRows(Grid.currentBlockList)
        }
      case KeyCode.Down =>
        if (gameStarted) {
          val (newBlockList, newTetrominus) = Grid.currentTetrominus.moveDown(Grid.currentBlockList)
          Grid.currentBlockList = newBlockList
          Grid.currentTetrominus = newTetrominus
          val (newBlockList2, newTetrominus2) = checkIfLost(Grid.currentBlockList)
          Grid.currentBlockList = newBlockList2
          Grid.currentTetrominus = newTetrominus2
          Grid.currentBlockList = updateDisplay(Grid.currentBlockList)
          Grid.currentBlockList = checkAndRemoveRows(Grid.currentBlockList)
        }
      case KeyCode.Up =>
        if (gameStarted) {
          val (newBlockList, newTetrominus) = Grid.currentTetrominus.rotate(Grid.currentBlockList)
          Grid.currentBlockList = newBlockList
          Grid.currentTetrominus = newTetrominus
          val (newBlockList2, newTetrominus2) = checkIfLost(Grid.currentBlockList)
          Grid.currentBlockList = newBlockList2
          Grid.currentTetrominus = newTetrominus2
          Grid.currentBlockList = updateDisplay(Grid.currentBlockList)
          Grid.currentBlockList = checkAndRemoveRows(Grid.currentBlockList)
        }
      case KeyCode.Space =>
        if (gameStarted) {
          val (newBlockList, newTetrominus) = Grid.currentTetrominus.fullMoveDown(Grid.currentBlockList)
          Grid.currentBlockList = newBlockList
          Grid.currentTetrominus = newTetrominus
          val (newBlockList2, newTetrominus2) = checkIfLost(Grid.currentBlockList)
          Grid.currentBlockList = newBlockList2
          Grid.currentTetrominus = newTetrominus2
          Grid.currentBlockList = updateDisplay(Grid.currentBlockList)
          Grid.currentBlockList = checkAndRemoveRows(Grid.currentBlockList)
        }
      case _ =>
    }
  }

  drawGrid()
  drawBlocks(Grid.currentBlockList)
  startGame()
}
