package logic

import scala.util.Random

object Grid {
  val cols = 10
  val rows = 25

  private val random = new Random()

  private val tetrominusGenerators: List[() => Tetrominus] = List(
    () => ITetrominus(List(Block(4, 0), Block(4, 1), Block(4, 2), Block(4, 3)), 0),
    () => JTetrominus(List(Block(5, 0), Block(5, 1), Block(5, 2), Block(4, 2)), 0),
    () => LTetrominus(List(Block(4, 0), Block(4, 1), Block(4, 2), Block(5, 2)), 0),
    () => OTetrominus(List(Block(4, 0), Block(4, 1), Block(5, 0), Block(5, 1)), 0),
    () => STetrominus(List(Block(4, 1), Block(5, 1), Block(5, 0), Block(6, 0)), 0),
    () => TTetrominus(List(Block(4, 0), Block(5, 0), Block(6, 0), Block(5, 1)), 0),
    () => ZTetrominus(List(Block(4, 0), Block(5, 0), Block(5, 1), Block(6, 1)), 0)
  )

  var currentTetrominus: Tetrominus = tetrominusGenerators(random.nextInt(tetrominusGenerators.length))()
  var currentBlockList: List[Block] = List()

  def addBlocksToList(blockList: List[Block], blocks: List[Block]): List[Block] = {
    blockList ++ blocks
  }

  def removeBlocksFromList(blockList: List[Block], blocks: List[Block]): List[Block] = {
    blockList.filter(b => !blocks.contains(b))
  }

  def nextTetrominus(blockList: List[Block]): (List[Block], Tetrominus) = {
    val nextIndex = random.nextInt(tetrominusGenerators.length)
    val tetrominus = tetrominusGenerators(nextIndex)()
    (Grid.addBlocksToList(blockList, tetrominus.blocks), tetrominus)
  }
}
