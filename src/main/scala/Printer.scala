object Printer {

  val tetrisSmall = s"${Console.BOLD}  ___ _____ _ ___ __\n   | |_  | |_) | (_ \n   | |__ | | \\_|___)${Console.RESET}"

  def apply(board: List[Tetromino], bW: Int, nextTet: List[Tetromino])={
    clear()
    printTitle
    previewBox(nextTet)
    printBoard(board, 10)
  }

  def printTitle ={
    println(tetrisSmall + "\n")
  }

  def printBoard(board: List[Tetromino], bW: Int): Unit = {
    val printable = boardConverter(board, bW)

    println("┏" + "━" * bW * 2 + "━┓")
    printable.foreach(row => {
      print("┃ ")
      row.foreach(cell => {
        print(cell + " ")
      })
      println("┃")
    })
    println("┗" + "━" * bW * 2 + "━┛")
  }

  private def boardConverter(board: List[Tetromino], bW: Int): List[List[(String)]] = {
    val blank = List.range(0, 150).map(i => (i, s"${Console.BLACK}.${Console.RESET}"))

    val filled: List[String] =
      blank.map{case (index:Int, blankLook:String) =>
        if(board.map(_.getIndex).contains(index)) {
          val tetromino = board(board.indexWhere(p => p.getIndex == index))

          tetromino.getLook} else blankLook}

    filled.grouped(bW).toList
  }

  def previewBox(preview: List[Tetromino]) ={
    println(" Up next:")
    val spawnPos = List(-4,-5,-6,-7,-14,-15,-16,-17,-24,-25,-26,-27,-34,-35,-36,-37).reverse
    val ind: List[List[String]] = spawnPos.map(i => if(preview.map(_.getIndex).contains(i)) "■" else " ").grouped(4).toList

    println("┏━" + "━"*8 + "━┓" + s" ${preview.head.getShape}")
    ind.foreach { row => println(s"┃ ${row.mkString("", " ", " ")} ┃")}
    println("┗━" + "━"*8 + "━┛")
  }


  def clear(): Unit ={
    print("\033[H\033[2J")
    println("\n\n")
  }
}
