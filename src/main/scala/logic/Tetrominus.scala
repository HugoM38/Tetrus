package logic

case class Block(x: Int, y: Int)

object MoveDirection extends Enumeration {
  val Left, Right, Down = Value
}

sealed abstract class Tetrominus {
  def blocks: Array[Block]

  def state: Int

  def rotate: Tetrominus

  def isValidMoveHorizontal(blocks: Array[Block]): Boolean = {
    blocks.forall(b => b.x >= 0 && b.x <= 9)
  }

  def isValidMoveVertical(blocks: Array[Block]): Boolean = {
    blocks.forall(b => b.y >= 0 && b.y <= 19)
  }

  private def touchesOtherBlocksVertical: Boolean = {
    val filteredBlockList = Grid.blockList.filterNot(blocks.contains)
    blocks.exists(b => filteredBlockList.exists(bl => bl.x == b.x && bl.y == b.y + 1))
  }

  private def touchesOtherBlocksHorizontal(direction: MoveDirection.Value): Boolean = {
    val filteredBlockList = Grid.blockList.filterNot(blocks.contains)
    if (direction == MoveDirection.Left) {
      blocks.exists(b => filteredBlockList.exists(bl => bl.x == b.x - 1 && bl.y == b.y))
    } else {
      blocks.exists(b => filteredBlockList.exists(bl => bl.x == b.x + 1 && bl.y == b.y))
    }
  }

  def moveLeft: Tetrominus = move(MoveDirection.Left)

  def moveRight: Tetrominus = move(MoveDirection.Right)

  def moveDown: Tetrominus = move(MoveDirection.Down)


  private def move(direction: MoveDirection.Value): Tetrominus = {
    if (direction == MoveDirection.Left) {
      val newBlocks = blocks.map(b => b.copy(x = b.x - 1))
      if (isValidMoveHorizontal(newBlocks) && !touchesOtherBlocksHorizontal(direction)) {
        val blocksToRemove = blocks.filter(b => !newBlocks.contains(b))
        blocksToRemove.foreach(block => Grid.blockList = Grid.blockList.filter(b => b != block))
        newBlocks.foreach(block => Grid.blockList = Grid.blockList :+ block)
        this.copy(blocks = newBlocks, state)
      } else {
        this
      }
    } else if (direction == MoveDirection.Right) {
      val newBlocks = blocks.map(b => b.copy(x = b.x + 1))
      if (isValidMoveHorizontal(newBlocks) && !touchesOtherBlocksHorizontal(direction)) {
        val blocksToRemove = blocks.filter(b => !newBlocks.contains(b))
        blocksToRemove.foreach(block => Grid.blockList = Grid.blockList.filter(b => b != block))
        newBlocks.foreach(block => Grid.blockList = Grid.blockList :+ block)
        this.copy(blocks = newBlocks, state)
      } else {
        this
      }
    } else {
      val newBlocks = blocks.map(b => b.copy(y = b.y + 1))
      if (canMoveDown) {
        val blocksToRemove = blocks.filter(b => !newBlocks.contains(b))
        blocksToRemove.foreach(block => Grid.blockList = Grid.blockList.filter(b => b != block))
        newBlocks.foreach(block => Grid.blockList = Grid.blockList :+ block)
        this.copy(blocks = newBlocks, state)
      } else {
        Grid.nextTetrominus
      }
    }

  }

  def canMoveDown: Boolean = {
    val newBlocks = blocks.map(b => b.copy(y = b.y + 1))
    isValidMoveVertical(newBlocks) && !touchesOtherBlocksVertical
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

    val currentBlocksSet = blocks.toSet
    val blocksToCheck = Grid.blockList.filterNot(currentBlocksSet.contains)

    if (isValidMoveHorizontal(newBlocks) && isValidMoveVertical(newBlocks) && !newBlocks.exists(b => blocksToCheck.contains(b))) {
      val blocksToRemove = blocks.filter(b => !newBlocks.contains(b))
      blocksToRemove.foreach(block => Grid.blockList = Grid.blockList.filter(b => b != block))
      newBlocks.foreach(block => Grid.blockList = Grid.blockList :+ block)
      ITetrominus(newBlocks, (state + 1) % 2)
    } else this
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
    val currentBlocksSet = blocks.toSet
    val blocksToCheck = Grid.blockList.filterNot(currentBlocksSet.contains)

    if (isValidMoveHorizontal(newBlocks) && isValidMoveVertical(newBlocks) && !newBlocks.exists(b => blocksToCheck.contains(b))) {
      val blocksToRemove = blocks.filter(b => !newBlocks.contains(b))
      blocksToRemove.foreach(block => Grid.blockList = Grid.blockList.filter(b => b != block))
      newBlocks.foreach(block => Grid.blockList = Grid.blockList :+ block)
      LTetrominus(newBlocks, (state + 1) % 4)
    } else this
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
    val currentBlocksSet = blocks.toSet
    val blocksToCheck = Grid.blockList.filterNot(currentBlocksSet.contains)

    if (isValidMoveHorizontal(newBlocks) && isValidMoveVertical(newBlocks) && !newBlocks.exists(b => blocksToCheck.contains(b))) {
      val blocksToRemove = blocks.filter(b => !newBlocks.contains(b))
      blocksToRemove.foreach(block => Grid.blockList = Grid.blockList.filter(b => b != block))
      newBlocks.foreach(block => Grid.blockList = Grid.blockList :+ block)
      JTetrominus(newBlocks, (state + 1) % 4)
    } else this
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
    val currentBlocksSet = blocks.toSet
    val blocksToCheck = Grid.blockList.filterNot(currentBlocksSet.contains)

    if (isValidMoveHorizontal(newBlocks) && isValidMoveVertical(newBlocks) && !newBlocks.exists(b => blocksToCheck.contains(b))) {
      val blocksToRemove = blocks.filter(b => !newBlocks.contains(b))
      blocksToRemove.foreach(block => Grid.blockList = Grid.blockList.filter(b => b != block))
      newBlocks.foreach(block => Grid.blockList = Grid.blockList :+ block)
      STetrominus(newBlocks, (state + 1) % 2)
    } else this
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
    val currentBlocksSet = blocks.toSet
    val blocksToCheck = Grid.blockList.filterNot(currentBlocksSet.contains)

    if (isValidMoveHorizontal(newBlocks) && isValidMoveVertical(newBlocks) && !newBlocks.exists(b => blocksToCheck.contains(b))) {
      val blocksToRemove = blocks.filter(b => !newBlocks.contains(b))
      blocksToRemove.foreach(block => Grid.blockList = Grid.blockList.filter(b => b != block))
      newBlocks.foreach(block => Grid.blockList = Grid.blockList :+ block)
      TTetrominus(newBlocks, (state + 1) % 4)
    }
    else this
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
    val currentBlocksSet = blocks.toSet
    val blocksToCheck = Grid.blockList.filterNot(currentBlocksSet.contains)

    if (isValidMoveHorizontal(newBlocks) && isValidMoveVertical(newBlocks) && !newBlocks.exists(b => blocksToCheck.contains(b))) {
      val blocksToRemove = blocks.filter(b => !newBlocks.contains(b))
      blocksToRemove.foreach(block => Grid.blockList = Grid.blockList.filter(b => b != block))
      newBlocks.foreach(block => Grid.blockList = Grid.blockList :+ block)
      ZTetrominus(newBlocks, (state + 1) % 2)
    } else this
  }

  override def copy(blocks: Array[Block], state: Int = this.state): ZTetrominus = ZTetrominus(blocks, state)
}
