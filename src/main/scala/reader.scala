object reader {

  def main(args: Array[String]): Unit = {
    val con = new jline.console.ConsoleReader
    val is = con.getInput
    val nbis = new jline.internal.NonBlockingInputStream(is, true)
    val charStream = Stream.iterate(0)(x => nbis.read(100))

    charStream.foldLeft("")((string ,key) => {
      val ns: String = string
      val userInput: Char = key.toChar
      var tick = 0
      clear
      println("\n\n")
      println(userInput)
      printBoard(board,10, 15)
      println(board.head.getIndex())
      parse(board, userInput, tick)
      ns
    })

  }

  val board: List[Tetromino] = List(new Tetromino("dot", "red", 15))



  def parse(b: List[Tetromino],userInput: Char, tick: Int) ={
    if (tick%5 == 0){
      b.foreach(t => t.fall(10))}
    b.foreach(t =>
    userInput match {
      case 'a' => t.moveLeft()
      case 'd' => t.moveRight()
      case _   =>
    })
  }





  def clear ={
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
      (thing.getIndex, 2, thing.getLook())
    } else s)
    steps.map(s => filled.slice(0, s).takeRight(bW))
  }

}
