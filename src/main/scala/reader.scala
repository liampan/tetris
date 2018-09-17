object reader {

  def main(args: Array[String]): Unit = {
    val con = new jline.console.ConsoleReader
    val is = con.getInput
    val nbis = new jline.internal.NonBlockingInputStream(is, true)
    val charStream = Stream.iterate(0)(x => nbis.read(1000))

    charStream.foldLeft("")((string ,key) => {
      val ns = string
      clear
      println("\n\n")
      println(key.toChar)
      printBoard(Nil,10, 15)
      ns
    })

  }

  val board: List[Tetromino] = List(new Tetromino("dot", "red", 5))







  def clear ={
    print("\033[H\033[2J")
  }


  def printBoard(board: List[Int], bW: Int, bH: Int): Unit = {
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

  def boardConverter(things: List[Int], bW: Int, bH: Int): List[List[(Int, Int, String)]] = {
    val blank = List.range(0, bW*bH).map(i => (i, 0, "blank"))
    val steps = List.range(bW, bH*bW+bW, bW)
    val filled: List[(Int, Int, String)] = blank
    steps.map(s => filled.slice(0, s).takeRight(bW))
  }

}
