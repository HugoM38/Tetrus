package logic

case class Block(x: Int, y: Int)

object MoveDirection extends Enumeration {
  val Left, Right, Down = Value
}

sealed abstract class Tetrominus {
  def blocks: Array[Block]

  def state: Int

  def rotate: Tetrominus

  private def isValidMove(blocks: Array[Block]): Boolean =
    isValidMoveHorizontal(blocks) && isValidMoveVertical(blocks)

  private def isValidMoveHorizontal(blocks: Array[Block]): Boolean =
    blocks.forall(b => b.x >= 0 && b.x <= 9)

  private def isValidMoveVertical(blocks: Array[Block]): Boolean =
    blocks.forall(b => b.y >= 0 && b.y <= 24)

  private def touchesOtherBlocksVertical: Boolean = {
    val filteredBlockList = Grid.blockList.filterNot(blocks.contains)
    blocks.exists(b => filteredBlockList.exists(bl => bl.x == b.x && bl.y == b.y + 1))
  }

  private def touchesOtherBlocksHorizontal(direction: MoveDirection.Value): Boolean = {
    if (direction == MoveDirection.Down) return false
    val filteredBlockList = Grid.blockList.filterNot(blocks.contains)
    val dx = if (direction == MoveDirection.Left) -1 else 1
    blocks.exists(b => filteredBlockList.exists(bl => bl.x == b.x + dx && bl.y == b.y))
  }

  def moveLeft: Tetrominus = move(MoveDirection.Left)

  def moveRight: Tetrominus = move(MoveDirection.Right)

  def moveDown: Tetrominus = move(MoveDirection.Down)

  def fullMoveDown: Tetrominus = move()

  private def move(direction: MoveDirection.Value): Tetrominus = {
    val dx = if (direction == MoveDirection.Left) -1 else if (direction == MoveDirection.Right) 1 else 0
    val dy = if (direction == MoveDirection.Down) 1 else 0

    val newBlocks = blocks.map(b => b.copy(x = b.x + dx, y = b.y + dy))
    val isValidMove = isValidMoveHorizontal(newBlocks) && isValidMoveVertical(newBlocks)

    if (isValidMove && !touchesOtherBlocksHorizontal(direction) && !touchesOtherBlocksVertical) {
      val blocksToRemove = blocks.filter(b => !newBlocks.contains(b))
      blocksToRemove.foreach(block => Grid.blockList = Grid.blockList.filter(b => b != block))
      newBlocks.foreach(block => Grid.blockList = Grid.blockList :+ block)
      this.copy(blocks = newBlocks, state)
    } else if (direction == MoveDirection.Down) {
      Grid.nextTetrominus
    } else {
      this
    }
  }

  private def move(): Tetrominus = {
    var result = move(MoveDirection.Down)
    while (result.canMoveDown) {
      result = result.move(MoveDirection.Down)
    }
    result
  }

  def updateBlocks(newBlocks: Array[Block], state: Int): Tetrominus = {
    val currentBlocksSet = blocks.toSet
    val blocksToCheck = Grid.blockList.filterNot(currentBlocksSet.contains)

    if (isValidMove(newBlocks) && !newBlocks.exists(b => blocksToCheck.contains(b))) {
      val blocksToRemove = blocks.filter(b => !newBlocks.contains(b))
      blocksToRemove.foreach(block => Grid.blockList = Grid.blockList.filter(b => b != block))
      newBlocks.foreach(block => Grid.blockList = Grid.blockList :+ block)
      copy(newBlocks, state)
    } else this
  }

  def canMoveDown: Boolean = {
    val newBlocks = blocks.map(b => b.copy(y = b.y + 1))
    isValidMoveVertical(newBlocks) && !touchesOtherBlocksVertical
  }

  private def copy(blocks: Array[Block], i: Int): Tetrominus = {
    this match {
      case ITetrominus(_, _) => ITetrominus(blocks, i)
      case LTetrominus(_, _) => LTetrominus(blocks, i)
      case JTetrominus(_, _) => JTetrominus(blocks, i)
      case OTetrominus(_, _) => OTetrominus(blocks, i)
      case STetrominus(_, _) => STetrominus(blocks, i)
      case TTetrominus(_, _) => TTetrominus(blocks, i)
      case ZTetrominus(_, _) => ZTetrominus(blocks, i)
    }
  }
}


case class ITetrominus(override val blocks: Array[Block], override val state: Int) extends Tetrominus {
  override def rotate: Tetrominus = {
    val center = blocks(1)
    val newBlocks = state match {
      case 0 => Array(Block(center.x - 1, center.y), Block(center.x, center.y), Block(center.x + 1, center.y), Block(center.x + 2, center.y))
      case 1 => Array(Block(center.x, center.y - 1), Block(center.x, center.y), Block(center.x, center.y + 1), Block(center.x, center.y + 2))
    }
    updateBlocks(newBlocks, (state + 1) % 2)
  }
}

case class LTetrominus(override val blocks: Array[Block], override val state: Int) extends Tetrominus {

  override def rotate: Tetrominus = {
    val center = blocks(1)
    val newBlocks = state match {
      case 0 => Array(Block(center.x - 1, center.y), center, Block(center.x + 1, center.y), Block(center.x - 1, center.y + 1))
      case 1 => Array(Block(center.x, center.y - 1), center, Block(center.x, center.y + 1), Block(center.x - 1, center.y - 1))
      case 2 => Array(Block(center.x - 1, center.y), center, Block(center.x + 1, center.y), Block(center.x + 1, center.y - 1))
      case 3 => Array(Block(center.x, center.y - 1), center, Block(center.x, center.y + 1), Block(center.x + 1, center.y + 1))
    }
    updateBlocks(newBlocks, (state + 1) % 4)
  }
}

case class JTetrominus(override val blocks: Array[Block], override val state: Int) extends Tetrominus {
  override def rotate: Tetrominus = {
    val center = blocks(1)
    val newBlocks = state match {
      case 0 => Array(Block(center.x - 1, center.y), center, Block(center.x + 1, center.y), Block(center.x - 1, center.y - 1))
      case 1 => Array(Block(center.x, center.y - 1), center, Block(center.x, center.y + 1), Block(center.x + 1, center.y - 1))
      case 2 => Array(Block(center.x - 1, center.y), center, Block(center.x + 1, center.y), Block(center.x + 1, center.y + 1))
      case 3 => Array(Block(center.x, center.y - 1), center, Block(center.x, center.y + 1), Block(center.x - 1, center.y + 1))
    }
    updateBlocks(newBlocks, (state + 1) % 4)
  }
}

case class OTetrominus(override val blocks: Array[Block], override val state: Int) extends Tetrominus {
  override def rotate: OTetrominus = this
}

case class STetrominus(override val blocks: Array[Block], override val state: Int) extends Tetrominus {
  override def rotate: Tetrominus = {
    val center = blocks(1)
    val newBlocks = state match {
      case 0 => Array(Block(center.x - 1, center.y - 1), center, Block(center.x - 1, center.y), Block(center.x, center.y + 1))
      case 1 => Array(Block(center.x + 1, center.y), center, Block(center.x, center.y + 1), Block(center.x - 1, center.y + 1))
    }
    updateBlocks(newBlocks, (state + 1) % 2)
  }
}

case class TTetrominus(override val blocks: Array[Block], override val state: Int) extends Tetrominus {
  override def rotate: Tetrominus = {
    val center = blocks(1)
    val newBlocks = state match {
      case 0 => Array(Block(center.x, center.y - 1), center, Block(center.x, center.y + 1), Block(center.x - 1, center.y))
      case 1 => Array(Block(center.x - 1, center.y), center, Block(center.x + 1, center.y), Block(center.x, center.y - 1))
      case 2 => Array(Block(center.x, center.y + 1), center, Block(center.x, center.y - 1), Block(center.x + 1, center.y))
      case 3 => Array(Block(center.x + 1, center.y), center, Block(center.x - 1, center.y), Block(center.x, center.y + 1))
    }
    updateBlocks(newBlocks, (state + 1) % 4)
  }
}

case class ZTetrominus(override val blocks: Array[Block], override val state: Int) extends Tetrominus {
  override def rotate: Tetrominus = {
    val center = blocks(1)
    val newBlocks = state match {
      case 0 => Array(Block(center.x, center.y + 1), center, Block(center.x + 1, center.y), Block(center.x + 1, center.y - 1))
      case 1 => Array(Block(center.x - 1, center.y), center, Block(center.x, center.y + 1), Block(center.x + 1, center.y + 1))
    }
    updateBlocks(newBlocks, (state + 1) % 2)
  }
}