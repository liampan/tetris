
object reader {

  var nextTet: List[Tetromino] = List()

  var presTet: List[Tetromino] = List()

  def main(args: Array[String]): Unit = {
    val con = new jline.console.ConsoleReader
    val is = con.getInput
    val nbis = new jline.internal.NonBlockingInputStream(is, true)
    val charStream = Stream.iterate(0)(x => nbis.read(100))
    var tick = 0

    charStream.foldLeft(spawn(tick))((currentBoard, key) => {
      val userInput: Char = key.toChar
      clear()
      println("\n\n")
      previewBox()
      printBoard(currentBoard,10, 15)
      println(tick)
      gameOver(currentBoard)
      val nb = parse(currentBoard, userInput, tick)
      tick +=1
      nb
    })

  }

  def parse(board: List[Tetromino], userInput: Char, tick: Int): List[Tetromino] ={
    if (tick%5 == 0){board.foreach(t => t.fall(10))}

    board.foreach(t => moveTetromino(t, userInput, board))

    collision(board)

    fullLineCheck(board) ++ spawn(tick)
  }

  def moveTetromino(block: Tetromino, userInput: Char, board: List[Tetromino]): Unit = {
    userInput match {
      case 'a' => block.moveLeft(board)
      case 'd' => block.moveRight(board)
      case 'w' => //rotate
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

  def gameOver(board: List[Tetromino]): Unit ={
    if (board.exists(t => t.getIndex/10 == 0 && !t.userCanControl && !t.canFall )) System.exit(0)
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

    if (playerTets.exists(t => (t.getIndex-1)%10 == 9 || t.getIndex == 0)){board.foreach(_.canMoveLeft = false)}
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

  def clear(): Unit ={
    print("\033[H\033[2J")
  }

  def printBoard(board: List[Tetromino], bW: Int, bH: Int): Unit = {
    val printable = boardConverter(board, bW, bH)
    println("┏" + "━" * bW * 2 + "━┓")
    printable.foreach(row => {
      print("┃ ")
      row.foreach(cell => {
        print(s"${if (cell._2 == 0) s"${Console.BLACK}.${Console.RESET}" else cell._3} ")
      })
      println("┃")
    })
    println("┗" + "━" * bW * 2 + "━┛")
  }

  def boardConverter(things: List[Tetromino], bW: Int, bH: Int): List[List[(Int, Int, String)]] = {
    val blank = List.range(0, bW*bH).map(i => (i, 0, "blank"))
    val steps = List.range(bW, bH*bW+bW, bW)
    val filled: List[(Int, Int, String)] = blank.map(s =>  if(things.map(_.getIndex).contains(s._1)) {
      val thing = things(things.indexWhere(p => p.getIndex == s._1))
      (thing.getIndex, 2, thing.getLook)
    } else s)
    steps.map(s => filled.slice(0, s).takeRight(bW))
  }



  def previewBox() ={
    clear()
    println()
    println(tetrisSmall)
    println()
    println(" Up next:")
    val preview = nextTet
    val spawnPos = List(-4,-5,-6,-7,-14,-15,-16,-17,-24,-25,-26,-27,-34,-35,-36,-37)
    val ind: List[List[String]] = spawnPos.map(i => if(nextTet.map(_.getIndex).contains(i)) "■" else " ").grouped(4).toList

    println("┏━" + "━"*8 + "━┓")
    ind.foreach { row => println(s"┃ ${row.mkString("", " ", " ")} ┃")}
    println("┗━" + "━"*8 + "━┛")
  }

  val tetrisSmall = s"${Console.BOLD}  ___ _____ _ ___ __\n   | |_  | |_) | (_ \n   | |__ | | \\_|___)${Console.RESET}"


}
