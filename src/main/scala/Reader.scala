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

    val yyy = if (tick%5 == 0){blocks.map(t => t.fall(10))} else blocks

    val movedBoard = board.copy(blocks = moveTetromino(userInput, yyy))

    val xxx = collision(movedBoard)

    xxx
      .gameOver.map(t => t)
      .fold(_ => board, board =>
        board).spawn
             .fullLineCheck
             .tickOne
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

  def collision(board: Board): Board ={
    val blocks = board.blocks

    val movingTets = blocks.filter(t =>  t.canFall)
    val playerTets = blocks.filter(t =>  t.userCanControl)
    val uncontrolledTetsIndexes = blocks.filterNot(_.userCanControl).map(_.getIndex)
    val nonMovingTetsIndexes = blocks.filterNot(t =>  t.canFall).map(_.getIndex).toSet
    val movingTetsFutureIndexes = blocks.filter(t =>  t.canFall).map(_.getIndex+10).toSet


    val a = blocks.map(t => if(t.getIndex+10>149) t.freeze else t)
    //this block has hit the bottom so freezes

    val b = if (nonMovingTetsIndexes.intersect(movingTetsFutureIndexes).nonEmpty){a.map(_.freeze)} else a
    //a moving block has hit a frozen block

    val c = if (playerTets.exists(t => (t.getIndex+1)%10 == 0)){b.map(t => t.copy(canMoveRight = false))}else b
    //something has hit the right wall, nothing can move right

    val d = if (playerTets.exists(t => (t.getIndex-1)%10 == 9 || (t.getIndex<0) && (t.getIndex-1)%10 == -1 || t.getIndex == 0))
    {c.map(_.copy(canMoveLeft = false))} else c
    //something has hit the left wall, nothing can move left

    val e = if (playerTets.exists(t => uncontrolledTetsIndexes.contains(t.getIndex-1))){d.map(_.copy(canMoveLeft = false))} else d
    //something has hit a block to its left

    val f = if (playerTets.exists(t => uncontrolledTetsIndexes.contains(t.getIndex+1))){e.map(_.copy(canMoveRight = false))} else e
    //something has hit a block to its right
    board.copy(blocks = f)
  }
}
