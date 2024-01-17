package logic

object Grid {
  val cols = 10
  val rows = 20

  private val grid = Array.ofDim[Int](rows, cols)

  var currentTetrominus: ITetrominus = ITetrominus(Array(Block(0,0), Block(0,1), Block(0,2), Block(0,3)), 0)


  for (i <- 0 until rows; j <- 0 until cols) {
    grid(i)(j) = 0
  }
}
