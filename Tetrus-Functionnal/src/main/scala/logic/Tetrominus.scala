package logic

import scala.annotation.tailrec

case class Block(x: Int, y: Int)

object MoveDirection extends Enumeration {
  val Left, Right, Down = Value
}

sealed abstract class Tetrominus {
  def blocks: List[Block]

  def state: Int

  def rotate(blockList: List[Block]): (List[Block], Tetrominus)

  private def isValidMove(blocks: List[Block]): Boolean =
    isValidMoveHorizontal(blocks) && isValidMoveVertical(blocks)

  private def isValidMoveHorizontal(blocks: List[Block]): Boolean =
    blocks.forall(b => b.x >= 0 && b.x <= 9)

  private def isValidMoveVertical(blocks: List[Block]): Boolean =
    blocks.forall(b => b.y >= 0 && b.y <= 24)

  private def touchesOtherBlocksVertical(blockList: List[Block]): Boolean = {
    val filteredBlockList = Grid.removeBlocksFromList(blockList, blocks)
    blocks.exists(b => filteredBlockList.exists(bl => bl.x == b.x && bl.y == b.y + 1))
  }

  private def touchesOtherBlocksHorizontal(direction: MoveDirection.Value, blockList: List[Block]): Boolean = {
    if (direction == MoveDirection.Down) return false
    val filteredBlockList = Grid.removeBlocksFromList(blockList, blocks)
    val dx = if (direction == MoveDirection.Left) -1 else 1
    blocks.exists(b => filteredBlockList.exists(bl => bl.x == b.x + dx && bl.y == b.y))
  }

  def moveLeft(blockList: List[Block]): (List[Block], Tetrominus) = move(MoveDirection.Left, blockList)

  def moveRight(blockList: List[Block]): (List[Block], Tetrominus) = move(MoveDirection.Right, blockList)

  def moveDown(blockList: List[Block]): (List[Block], Tetrominus) = move(MoveDirection.Down, blockList)

  def fullMoveDown(blockList: List[Block]): (List[Block], Tetrominus) = move(blockList)

  private def move(direction: MoveDirection.Value, blockList: List[Block]): (List[Block], Tetrominus) = {
    val dx = if (direction == MoveDirection.Left) -1 else if (direction == MoveDirection.Right) 1 else 0
    val dy = if (direction == MoveDirection.Down) 1 else 0

    val newBlocks = blocks.map(b => b.copy(x = b.x + dx, y = b.y + dy))
    val isValidMove = isValidMoveHorizontal(newBlocks) && isValidMoveVertical(newBlocks)

    if (isValidMove && !touchesOtherBlocksHorizontal(direction, blockList) && !touchesOtherBlocksVertical(blockList)) {
      val blocksToRemove = blocks.filter(b => !newBlocks.contains(b))
      val newBlockList = Grid.removeBlocksFromList(blockList, blocksToRemove)
      (Grid.addBlocksToList(newBlockList, newBlocks), this.copy(blocks = newBlocks, state))
    } else if (direction == MoveDirection.Down) {
      Grid.nextTetrominus(blockList)
    } else {
      (blockList, this)
    }
  }

  private def move(blockList: List[Block]): (List[Block], Tetrominus) = {
    @tailrec
    def moveDown(currentBlockList: List[Block], currentTetrominus: Tetrominus): (List[Block], Tetrominus) = {
      if (currentTetrominus.canMoveDown(currentBlockList)) {
        val (newBlockList, newTetrominus) = currentTetrominus.move(MoveDirection.Down, currentBlockList)
        moveDown(newBlockList, newTetrominus)
      } else {
        (currentBlockList, currentTetrominus)
      }
    }

    val (initialBlockList, initialTetrominus) = move(MoveDirection.Down, blockList)
    moveDown(initialBlockList, initialTetrominus)
  }

  def updateBlocks(blockList: List[Block], newBlocks: List[Block], state: Int): (List[Block], Tetrominus) = {
    val blocksToCheck = Grid.removeBlocksFromList(blockList, blocks)

    if (isValidMove(newBlocks) && !newBlocks.exists(b => blocksToCheck.contains(b))) {
      val blocksToRemove = blocks.filter(b => !newBlocks.contains(b))
      val newBlockList = Grid.removeBlocksFromList(blockList, blocksToRemove)
      (Grid.addBlocksToList(newBlockList, newBlocks), this.copy(blocks = newBlocks, state))
    } else (blockList, this)
  }

  def canMoveDown(blockList: List[Block]): Boolean = {
    val newBlocks = blocks.map(b => b.copy(y = b.y + 1))
    isValidMoveVertical(newBlocks) && !touchesOtherBlocksVertical(blockList)
  }

  private def copy(blocks: List[Block], i: Int): Tetrominus = {
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


case class ITetrominus(override val blocks: List[Block], override val state: Int) extends Tetrominus {
  override def rotate(blockList: List[Block]): (List[Block], Tetrominus) = {
    val center = blocks(1)
    val newBlocks = state match {
      case 0 => List(Block(center.x - 1, center.y), Block(center.x, center.y), Block(center.x + 1, center.y), Block(center.x + 2, center.y))
      case 1 => List(Block(center.x, center.y - 1), Block(center.x, center.y), Block(center.x, center.y + 1), Block(center.x, center.y + 2))
    }
    updateBlocks(blockList, newBlocks, (state + 1) % 2)
  }
}

case class LTetrominus(override val blocks: List[Block], override val state: Int) extends Tetrominus {

  override def rotate(blockList: List[Block]): (List[Block], Tetrominus) = {
    val center = blocks(1)
    val newBlocks = state match {
      case 0 => List(Block(center.x - 1, center.y), center, Block(center.x + 1, center.y), Block(center.x - 1, center.y + 1))
      case 1 => List(Block(center.x, center.y - 1), center, Block(center.x, center.y + 1), Block(center.x - 1, center.y - 1))
      case 2 => List(Block(center.x - 1, center.y), center, Block(center.x + 1, center.y), Block(center.x + 1, center.y - 1))
      case 3 => List(Block(center.x, center.y - 1), center, Block(center.x, center.y + 1), Block(center.x + 1, center.y + 1))
    }
    updateBlocks(blockList, newBlocks, (state + 1) % 4)
  }
}

case class JTetrominus(override val blocks: List[Block], override val state: Int) extends Tetrominus {
  override def rotate(blockList: List[Block]): (List[Block], Tetrominus) = {
    val center = blocks(1)
    val newBlocks = state match {
      case 0 => List(Block(center.x - 1, center.y), center, Block(center.x + 1, center.y), Block(center.x - 1, center.y - 1))
      case 1 => List(Block(center.x, center.y - 1), center, Block(center.x, center.y + 1), Block(center.x + 1, center.y - 1))
      case 2 => List(Block(center.x - 1, center.y), center, Block(center.x + 1, center.y), Block(center.x + 1, center.y + 1))
      case 3 => List(Block(center.x, center.y - 1), center, Block(center.x, center.y + 1), Block(center.x - 1, center.y + 1))
    }
    updateBlocks(blockList, newBlocks, (state + 1) % 4)
  }
}

case class OTetrominus(override val blocks: List[Block], override val state: Int) extends Tetrominus {
  override def rotate(blockList: List[Block]): (List[Block], Tetrominus) = (blockList, this)
}

case class STetrominus(override val blocks: List[Block], override val state: Int) extends Tetrominus {
  override def rotate(blockList: List[Block]): (List[Block], Tetrominus) = {
    val center = blocks(1)
    val newBlocks = state match {
      case 0 => List(Block(center.x - 1, center.y - 1), center, Block(center.x - 1, center.y), Block(center.x, center.y + 1))
      case 1 => List(Block(center.x + 1, center.y), center, Block(center.x, center.y + 1), Block(center.x - 1, center.y + 1))
    }
    updateBlocks(blockList, newBlocks, (state + 1) % 2)
  }
}

case class TTetrominus(override val blocks: List[Block], override val state: Int) extends Tetrominus {
  override def rotate(blockList: List[Block]): (List[Block], Tetrominus) = {
    val center = blocks(1)
    val newBlocks = state match {
      case 0 => List(Block(center.x, center.y - 1), center, Block(center.x, center.y + 1), Block(center.x - 1, center.y))
      case 1 => List(Block(center.x - 1, center.y), center, Block(center.x + 1, center.y), Block(center.x, center.y - 1))
      case 2 => List(Block(center.x, center.y + 1), center, Block(center.x, center.y - 1), Block(center.x + 1, center.y))
      case 3 => List(Block(center.x + 1, center.y), center, Block(center.x - 1, center.y), Block(center.x, center.y + 1))
    }
    updateBlocks(blockList, newBlocks, (state + 1) % 4)
  }
}

case class ZTetrominus(override val blocks: List[Block], override val state: Int) extends Tetrominus {
  override def rotate(blockList: List[Block]): (List[Block], Tetrominus) = {
    val center = blocks(1)
    val newBlocks = state match {
      case 0 => List(Block(center.x, center.y + 1), center, Block(center.x + 1, center.y), Block(center.x + 1, center.y - 1))
      case 1 => List(Block(center.x - 1, center.y), center, Block(center.x, center.y + 1), Block(center.x + 1, center.y + 1))
    }
    updateBlocks(blockList, newBlocks, (state + 1) % 2)
  }
}