
object reader {


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

    moveTetromino(userInput, blocks)

    collision(blocks)

    gameOver(blocks)

    val b = spawn(board)

    fullLineCheck(b).tickOne
  }

  def moveTetromino(userInput: Char, board: List[Tetromino]): Unit = {
    userInput match {
      case 'a' => board.foreach(tetromino => tetromino.moveLeft(board))
      case 'd' => board.foreach(tetromino => tetromino.moveRight(board))
      case 'w' => Rotate.rotate(board)
      case _   => //speeds upstream
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


  def collision(board: List[Tetromino]): Unit ={

    val movingTets = board.filter(t =>  t.canFall)
    val playerTets = board.filter(t =>  t.userCanControl)
    val uncontrolledTetsIndexes = board.filterNot(_.userCanControl).map(_.getIndex)
    val nonMovingTetsIndexes = board.filterNot(t =>  t.canFall).map(_.getIndex).toSet
    val movingTetsFutureIndexes = board.filter(t =>  t.canFall).map(_.getIndex+10).toSet


    board.foreach(t => if(t.getIndex+10>149) t.freeze)
    //this block has hit the bottom so freezes

    if (nonMovingTetsIndexes.intersect(movingTetsFutureIndexes).nonEmpty){board.foreach(_.freeze)}
    //a moving block has hit a frozen block

    if (playerTets.exists(t => (t.getIndex+1)%10 == 0)){board.foreach(_.canMoveRight = false)}
    //something has hit the right wall, nothing can move right

    if (playerTets.exists(t => (t.getIndex-1)%10 == 9 || (t.getIndex<0) && (t.getIndex-1)%10 == -1 || t.getIndex == 0))
    {board.foreach(_.canMoveLeft = false)}
    //something has hit the left wall, nothing can move left

    if (playerTets.exists(t => uncontrolledTetsIndexes.contains(t.getIndex-1))){board.foreach(_.canMoveLeft = false)}
    //something has hit a block to its left

    if (playerTets.exists(t => uncontrolledTetsIndexes.contains(t.getIndex+1))){board.foreach(_.canMoveRight = false)}
    //something has hit a block to its right

  }

  def spawn(board: Board): Board ={

    val tick = board.tick

    val spawnRate = 100
    if (tick%spawnRate == 0) {
      board.copy(
        blocks = board.blocks ++ board.nextTet,
        nextTet = Spawner.spawn
      )
    }
    else {
      board
    }
  }

  def gameOver(board: List[Tetromino]): Unit ={
    if (board.exists(t => t.getIndex/10 == 0 && !t.userCanControl && !t.canFall )) System.exit(0)
  }
}
