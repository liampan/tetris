object Reader {

  def main(args: Array[String]): Unit = {
    val con = new jline.console.ConsoleReader
    val is = con.getInput
    val nbis = new jline.internal.NonBlockingInputStream(is, true)
    val charStream = Stream.iterate(0)(x => nbis.read(100))

    charStream.foldLeft(Board(Nil))((currentBoard, key) => {
      val userInput: Char = key.toChar
      parse(currentBoard, userInput)
    })
  }

  def parse(board: Board, userInput: Char): Board ={
    val blocks = board.blocks
    val tick = board.tick

    board.print

    if (tick%5 == 0){blocks.foreach(t => t.fall(10))}

    val movedBoard = board.copy(blocks = moveTetromino(userInput, blocks))

    collision(movedBoard)

    board
      .gameOver
      .fold(_ => board, board =>
        board.spawn
             .fullLineCheck
             .tickOne)
  }

  def moveTetromino(userInput: Char, board: List[Tetromino]): List[Tetromino] = {
    val userControlledTets = board.filter(_.userCanControl)
    val staticTets = board.filterNot(_.userCanControl)
    userInput match {
      case 'a' => staticTets ++ userControlledTets.map(tetromino => tetromino.moveLeft(board))
      case 'd' => staticTets ++ userControlledTets.map(tetromino => tetromino.moveRight(board))
      case 'w' => Rotate.rotate(board)
      case _   => board
    }
  }

  def fullLineCheck(board: Board): Board ={
    val blocks = board.blocks
    val nonMovingTets = blocks.filterNot(_.userCanControl)
    val lines = nonMovingTets.map(_.getIndex).map(i => i/10)

    val mapped = lines.groupBy(i=>i).mapValues(_.length)

    val fullLines = mapped.mapValues(_ == 10).filter(i => i._2).keys.toList
    val increaseScoreBy = fullLines.length
    if (fullLines.nonEmpty) {
      val lowestLine = fullLines.max
      val aboveLine = blocks.filter(t => t.getIndex < lowestLine * 10)
      aboveLine.foreach(t => t.restartGravity)
    }
    board.copy(blocks = blocks.filterNot(t => fullLines.contains(t.getIndex/10)), score = board.score + increaseScoreBy)
  }


  def collision(board: Board): Unit ={
    val blocks = board.blocks

    val movingTets = blocks.filter(t =>  t.canFall)
    val playerTets = blocks.filter(t =>  t.userCanControl)
    val uncontrolledTetsIndexes = blocks.filterNot(_.userCanControl).map(_.getIndex)
    val nonMovingTetsIndexes = blocks.filterNot(t =>  t.canFall).map(_.getIndex).toSet
    val movingTetsFutureIndexes = blocks.filter(t =>  t.canFall).map(_.getIndex+10).toSet


    val a = blocks.map(t => if(t.getIndex+10>149) t.freeze else t)
    //this block has hit the bottom so freezes

    if (nonMovingTetsIndexes.intersect(movingTetsFutureIndexes).nonEmpty){blocks.foreach(_.freeze)}
    //a moving block has hit a frozen block

    if (playerTets.exists(t => (t.getIndex+1)%10 == 0)){blocks.foreach(_.canMoveRight = false)}
    //something has hit the right wall, nothing can move right

    if (playerTets.exists(t => (t.getIndex-1)%10 == 9 || (t.getIndex<0) && (t.getIndex-1)%10 == -1 || t.getIndex == 0))
    {blocks.foreach(_.canMoveLeft = false)}
    //something has hit the left wall, nothing can move left

    if (playerTets.exists(t => uncontrolledTetsIndexes.contains(t.getIndex-1))){blocks.foreach(_.canMoveLeft = false)}
    //something has hit a block to its left

    if (playerTets.exists(t => uncontrolledTetsIndexes.contains(t.getIndex+1))){blocks.foreach(_.canMoveRight = false)}
    //something has hit a block to its right

  }
}
