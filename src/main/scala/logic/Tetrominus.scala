package logic

case class Block(x: Int, y: Int)

sealed abstract class Tetrominus {
  def blocks: Array[Block]

  def state: Int
  def rotate: Tetrominus

  def isValidMove(blocks: Array[Block]): Boolean = {
    blocks.forall(b => b.x >= 0 && b.x <= 9 && b.y >= 0 && b.y <= 19)
  }

  def moveLeft: Tetrominus = {
    val newBlocks = blocks.map(b => b.copy(x = b.x - 1))
    if (isValidMove(newBlocks)) {
      this.copy(blocks = newBlocks, state)
    } else {
      this
    }
  }

  def moveRight: Tetrominus = {
    val newBlocks = blocks.map(b => b.copy(x = b.x + 1))
    if (isValidMove(newBlocks)) {
      this.copy(blocks = newBlocks, state)
    } else {
      this
    }
  }

  def moveDown: Tetrominus = {
    val newBlocks = blocks.map(b => b.copy(y = b.y + 1))
    if (isValidMove(newBlocks)) {
      this.copy(blocks = newBlocks, state)
    } else {
      Grid.nextTetrominus
    }
  }

  def copy(blocks: Array[Block], state: Int): Tetrominus
}

case class ITetrominus(override val blocks: Array[Block], override val state: Int) extends Tetrominus {

  def rotate: ITetrominus = {
    val center = blocks(1)

    val newBlocks = state match {
      case 0 => Array(Block(center.x - 1, center.y), Block(center.x, center.y), Block(center.x + 1, center.y), Block(center.x + 2, center.y))
      case 1 => Array(Block(center.x, center.y - 1), Block(center.x, center.y), Block(center.x, center.y + 1), Block(center.x, center.y + 2))
    }

    if (isValidMove(newBlocks)) ITetrominus(newBlocks, (state + 1) % 2) else this
  }

  override def copy(blocks: Array[Block], state: Int): Tetrominus = {
    ITetrominus(blocks, state)
  }
}
case class LTetrominus(override val blocks: Array[Block], override val state: Int) extends Tetrominus {
  override def rotate: LTetrominus = {
    val center = blocks(1)
    val newBlocks = state match {
      case 0 => Array(Block(center.x - 1, center.y), center, Block(center.x + 1, center.y), Block(center.x - 1, center.y + 1))
      case 1 => Array(Block(center.x, center.y - 1), center, Block(center.x, center.y + 1), Block(center.x - 1, center.y - 1))
      case 2 => Array(Block(center.x - 1, center.y), center, Block(center.x + 1, center.y), Block(center.x + 1, center.y - 1))
      case 3 => Array(Block(center.x, center.y - 1), center, Block(center.x, center.y + 1), Block(center.x + 1, center.y + 1))
    }
    if (isValidMove(newBlocks)) LTetrominus(newBlocks, (state + 1) % 4) else this
  }
  override def copy(blocks: Array[Block], state: Int = this.state): LTetrominus = LTetrominus(blocks, state)
}

case class JTetrominus(override val blocks: Array[Block], override val state: Int) extends Tetrominus {
  override def rotate: JTetrominus = {
    val center = blocks(1)
    val newBlocks = state match {
      case 0 => Array(Block(center.x - 1, center.y), center, Block(center.x + 1, center.y), Block(center.x - 1, center.y - 1))
      case 1 => Array(Block(center.x, center.y - 1), center, Block(center.x, center.y + 1), Block(center.x + 1, center.y - 1))
      case 2 => Array(Block(center.x - 1, center.y), center, Block(center.x + 1, center.y), Block(center.x + 1, center.y + 1))
      case 3 => Array(Block(center.x, center.y - 1), center, Block(center.x, center.y + 1), Block(center.x - 1, center.y + 1))
    }
    if (isValidMove(newBlocks)) JTetrominus(newBlocks, (state + 1) % 4) else this
  }
  override def copy(blocks: Array[Block], state: Int = this.state): JTetrominus = JTetrominus(blocks, state)
}

case class OTetrominus(override val blocks: Array[Block], override val state: Int) extends Tetrominus {
  def rotate: OTetrominus = {
    this
  }

  override def copy(blocks: Array[Block], state: Int): Tetrominus = {
    OTetrominus(blocks, state)
  }
}

case class STetrominus(override val blocks: Array[Block], override val state: Int) extends Tetrominus {
  override def rotate: STetrominus = {
    val center = blocks(1)
    val newBlocks = state match {
      case 0 => Array(Block(center.x - 1, center.y - 1), center, Block(center.x - 1, center.y), Block(center.x, center.y + 1))
      case 1 => Array(Block(center.x + 1, center.y), center, Block(center.x, center.y + 1), Block(center.x - 1, center.y + 1))
    }
    if (isValidMove(newBlocks)) STetrominus(newBlocks, (state + 1) % 2) else this
  }
  override def copy(blocks: Array[Block], state: Int = this.state): STetrominus = STetrominus(blocks, state)
}
case class TTetrominus(override val blocks: Array[Block], override val state: Int) extends Tetrominus {
  override def rotate: TTetrominus = {
    val center = blocks(1)
    val newBlocks = state match {
      case 0 => Array(Block(center.x, center.y - 1), center, Block(center.x, center.y + 1), Block(center.x - 1, center.y))
      case 1 => Array(Block(center.x - 1, center.y), center, Block(center.x + 1, center.y), Block(center.x, center.y - 1))
      case 2 => Array(Block(center.x, center.y + 1), center, Block(center.x, center.y - 1), Block(center.x + 1, center.y))
      case 3 => Array(Block(center.x + 1, center.y), center, Block(center.x - 1, center.y), Block(center.x, center.y + 1))
    }
    if (isValidMove(newBlocks)) TTetrominus(newBlocks, (state + 1) % 4) else this
  }
  override def copy(blocks: Array[Block], state: Int = this.state): TTetrominus = TTetrominus(blocks, state)
}

case class ZTetrominus(override val blocks: Array[Block], override val state: Int) extends Tetrominus {
  override def rotate: ZTetrominus = {
    val center = blocks(1)
    val newBlocks = state match {
      case 0 => Array(Block(center.x, center.y + 1), center, Block(center.x + 1, center.y), Block(center.x + 1, center.y - 1))
      case 1 => Array(Block(center.x - 1, center.y), center, Block(center.x, center.y + 1), Block(center.x + 1, center.y + 1))
    }
    if (isValidMove(newBlocks)) ZTetrominus(newBlocks, (state + 1) % 2) else this
  }
  override def copy(blocks: Array[Block], state: Int = this.state): ZTetrominus = ZTetrominus(blocks, state)
}