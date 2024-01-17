package logic

case class Block(x: Int, y: Int)

sealed abstract class Tetrominus {
  def blocks: Array[Block]

  def state: Int
  def rotate: Tetrominus

  def moveLeft: Tetrominus
  def moveRight: Tetrominus
  def moveDown: Tetrominus
}

case class ITetrominus(override val blocks: Array[Block], override val state: Int) extends Tetrominus {

  def rotate: ITetrominus = {
    val center = blocks(1)

    val newBlocks = state match {
      case 0 =>
        Array(
          Block(center.x - 1, center.y),
          Block(center.x, center.y),
          Block(center.x + 1, center.y),
          Block(center.x + 2, center.y)
        )
      case 1 =>
        Array(
          Block(center.x, center.y - 1),
          Block(center.x, center.y),
          Block(center.x, center.y + 1),
          Block(center.x, center.y + 2)
        )
      case 2 =>
        Array(
          Block(center.x + 1, center.y),
          Block(center.x, center.y),
          Block(center.x - 1, center.y),
          Block(center.x - 2, center.y)
        )
      case 3 =>
        Array(
          Block(center.x, center.y + 1),
          Block(center.x, center.y),
          Block(center.x, center.y - 1),
          Block(center.x, center.y - 2)
        )
    }

    if (newBlocks.forall(b => b.x >= 0 && b.x <= 9 && b.y >= 0 && b.y <= 19)) {
      val newState = (state + 1) % 4
      ITetrominus(newBlocks, newState)
    } else {
      this
    }
  }

  def moveLeft: ITetrominus = {
    val newTetrominus: ITetrominus = this.copy(blocks = blocks.map(b => b.copy(x = b.x - 1)))
    if (newTetrominus.blocks.forall(b => b.x >= 0)) {
      newTetrominus
    } else {
      this
    }
  }
  def moveRight: ITetrominus = {
    val newTetrominus: ITetrominus = this.copy(blocks = blocks.map(b => b.copy(x = b.x + 1)))
    if (newTetrominus.blocks.forall(b => b.x <= 9)) {
      newTetrominus
    } else {
      this
    }
  }
  def moveDown: ITetrominus = {
    val newTetrominus: ITetrominus = this.copy(blocks = blocks.map(b => b.copy(y = b.y + 1)))
    if (newTetrominus.blocks.forall(b => b.y <= 19)) {
      newTetrominus
    } else {
      this
    }
  }
}