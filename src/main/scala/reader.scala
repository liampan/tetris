
object reader {

  var nextTet: List[Tetromino] = Nil

  var presTet: List[Tetromino] = Nil

  def main(args: Array[String]): Unit = {
    val con = new jline.console.ConsoleReader
    val is = con.getInput
    val nbis = new jline.internal.NonBlockingInputStream(is, true)
    val charStream = Stream.iterate(0)(x => nbis.read(100))
    var tick = 0

    charStream.foldLeft(spawn(tick))((currentBoard, key) => {
      val userInput: Char = key.toChar
      val nb = parse(currentBoard, userInput, tick)
      tick +=1
      nb
    })

  }

  var gravity = true

  def parse(board: List[Tetromino], userInput: Char, tick: Int): List[Tetromino] ={
    Printer(board, 10, nextTet)

    if (tick%5 == 0 && gravity ){board.foreach(t => t.fall(10))}

    //board.foreach(t => moveTetromino(t, userInput, board))

    moveTetromino(userInput, board)

    println(if(presTet.nonEmpty){"*** "+ presTet.head.rotateState} else "--- ")

    collision(board)

    gameOver(board)

    fullLineCheck(board) ++ spawn(tick)
  }

  def moveTetromino(userInput: Char, board: List[Tetromino]): Unit = {
    userInput match {
      case 'a' => board.foreach(tetromino => tetromino.moveLeft(board))
      case 'd' => board.foreach(tetromino => tetromino.moveRight(board))
      case 'w' => Rotate.rotate(board)
      case 'g' => gravity = false
      case _   => //speeds upstream
    }
  }

  def fullLineCheck(board: List[Tetromino]): List[Tetromino] ={
    val nonMovingTets = board.filterNot(_.userCanControl)
    val lines = nonMovingTets.map(_.getIndex).map(i => i/10)

    val mapped = lines.groupBy(i=>i).mapValues(_.length)

    val fullLines = mapped.mapValues(_ == 10).filter(i => i._2).keys.toList
    if (fullLines.nonEmpty) {
      val lowestLine = fullLines.max
      val aboveLine = board.filter(t => t.getIndex < lowestLine * 10)
      aboveLine.foreach(t => t.restartGravity)
    }
    board.filterNot(t => fullLines.contains(t.getIndex/10))
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

  def spawn(tick: Int): List[Tetromino] ={
    val spawnRate = 100
    if (tick%spawnRate == 0) {
      presTet = nextTet
      nextTet = Spawner.spawn
      presTet
    }
    else {
      Nil
    }
  }

  def gameOver(board: List[Tetromino]): Unit ={
    if (board.exists(t => t.getIndex/10 == 0 && !t.userCanControl && !t.canFall )) System.exit(0)
  }
}
