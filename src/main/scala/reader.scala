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
      printBoard(Nil,20)
      ns
    })

  }


  def clear ={
    print("\033[H\033[2J")
  }


  def printBoard(board: List[Int], bS: Int): Unit = {
    val printable = boardConverter(board, bS)
    println("┏" + "━" * bS * 2 + "━┓")
    printable.foreach(row => {
      print("┃ ")
      row.foreach(cell => {
        print(s"${if (cell._2 == 0) s"${Console.BLACK}.${Console.RESET}" else cell._3} ")
      })
      println("┃")
    })
    println("┗" + "━" * bS * 2 + "━┛")
  }

  def boardConverter(things: List[Int], bS: Int): List[List[(Int, Int, String)]] = {
    val blank = List.range(0, 100).map(i => (i, 0, "blank"))
    val steps = List.range(bS, bS*bS+bS, bS)
    val filled: List[(Int, Int, String)] = blank
    steps.map(s => filled.slice(0, s).takeRight(bS))
  }

}
