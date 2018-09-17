import scala.util.Random

object reader {

  val emptyBoard: List[Tetromino] = List()

  def main(args: Array[String]): Unit = {
    val con = new jline.console.ConsoleReader
    val is = con.getInput
    val nbis = new jline.internal.NonBlockingInputStream(is, true)
    val charStream = Stream.iterate(0)(x => nbis.read(100))
    var tick = 0

    charStream.foldLeft(emptyBoard)((currentBoard, key) => {
      val userInput: Char = key.toChar
      clear()
      println("input : "+userInput)
      println("\n\n")
      printBoard(currentBoard,10, 15)
      println(tick)
      val nb = parse(currentBoard, userInput, tick)
      tick +=1
      nb
    })

  }

  def parse(board: List[Tetromino], userInput: Char, tick: Int): List[Tetromino] ={
    if (tick%10 == 0){board.foreach(t => t.fall(10))}

    board.foreach(t => moveTetromino(t, userInput))

    collison(board)

    board ++ spawn(tick)
  }

  def moveTetromino(block: Tetromino, userInput: Char): Unit = {
    userInput match {
      case 'a' => block.moveLeft
      case 'd' => block.moveRight
      case _   =>
    }
  }

  def collison(board: List[Tetromino]): Unit ={
    val movingTets = board.filter(t => t.canMove)
    val nonMovingTetsIndexes = board.filterNot(t => t.canMove).map(_.getIndex).toSet
    val movingTetsFutureIndexes = movingTets.map(_.getIndex+10).toSet
    if (nonMovingTetsIndexes.intersect(movingTetsFutureIndexes).nonEmpty){board.foreach(_.stopMove)}
  }

  def spawn(tick: Int): List[Tetromino] ={
    if (tick%150 == 0) {
      List(new Tetromino("dot", generateColour, 5))
    } else {
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


  def generateColour: String ={
    Random.nextInt(4) match {
      case 0 => "red"
      case 1 => "blue"
      case 2 => "green"
      case _ => "yellow"
    }
  }
}
