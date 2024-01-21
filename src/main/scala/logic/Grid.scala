package logic

object Grid {
  val cols = 10
  val rows = 20

  private val grid = Array.ofDim[Int](rows, cols)

  //var currentTetrominus: Tetrominus = ITetrominus(Array(Block(0,0), Block(0,1), Block(0,2), Block(0,3)), 0)
  //var currentTetrominus: Tetrominus = ZTetrominus(Array(Block(0,0), Block(1,0), Block(1,1), Block(2,1)), 0)
  //var currentTetrominus: Tetrominus = STetrominus(Array(Block(0,1), Block(1,1), Block(1,0), Block(2,0)), 0)
  //var currentTetrominus: Tetrominus = OTetrominus(Array(Block(0,0), Block(0,1), Block(1,0), Block(1,1)), 0)
  //var currentTetrominus: Tetrominus = LTetrominus(Array(Block(0,0), Block(0,1), Block(0,2), Block(1,2)), 0)
  //var currentTetrominus: Tetrominus = JTetrominus(Array(Block(1,0), Block(1,1), Block(1,2), Block(0,2)), 0)
  var currentTetrominus: Tetrominus = TTetrominus(Array(Block(0,0), Block(1,0), Block(2,0), Block(1,1)), 0)

  for (i <- 0 until rows; j <- 0 until cols) {
    grid(i)(j) = 0
  }
}
