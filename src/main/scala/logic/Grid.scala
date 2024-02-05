package logic
import scala.util.Random
object Grid {

  val cols = 10
  val rows = 25

  private val random = new Random()

  var blockList: Array[Block] = Array()

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

  def nextTetrominus: Tetrominus = {
    val nextIndex = random.nextInt(tetrominusGenerators.length)
    val tetrominus = tetrominusGenerators(nextIndex)()
    tetrominus.blocks.foreach(block => {
      blockList = blockList :+ block
    })
    tetrominus
  }

}
