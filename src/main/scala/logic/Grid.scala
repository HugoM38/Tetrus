package logic
import scala.util.Random

object Grid {
  val cols = 10
  val rows = 20

  private val grid = Array.ofDim[Int](rows, cols)
  private val random = new Random()

  private val tetrominusGenerators = Array(
    () => ITetrominus(Array(Block(4,0), Block(4,1), Block(4,2), Block(4,3)), 0),
    () => JTetrominus(Array(Block(5,0), Block(5,1), Block(5,2), Block(4,2)), 0),
    () => LTetrominus(Array(Block(4,0), Block(4,1), Block(4,2), Block(5,2)), 0),
    () => OTetrominus(Array(Block(4,0), Block(4,1), Block(5,0), Block(5,1)), 0),
    () => STetrominus(Array(Block(4,1), Block(5,1), Block(5,0), Block(6,0)), 0),
    () => TTetrominus(Array(Block(4,0), Block(5,0), Block(6,0), Block(5,1)), 0),
    () => ZTetrominus(Array(Block(4,0), Block(5,0), Block(5,1), Block(6,1)), 0)
  )

  var currentTetrominus: Tetrominus = nextTetrominus

  private def nextTetrominus: Tetrominus = {
    val nextIndex = random.nextInt(tetrominusGenerators.length)
    tetrominusGenerators(nextIndex)()
  }

  private def startGame(): Unit = {
    currentTetrominus = nextTetrominus
  }


  for (i <- 0 until rows; j <- 0 until cols) {
    grid(i)(j) = 0
  }

  startGame()
}
