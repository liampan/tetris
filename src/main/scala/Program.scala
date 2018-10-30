object Program {

  // Very JAVA-ry here...
  def main(args: Array[String]): Unit = {
    val con = new jline.console.ConsoleReader
    val is = con.getInput
    val nbis = new jline.internal.NonBlockingInputStream(is, true)
    val charStream = Stream.iterate(0)(_ => nbis.read(1000))

    charStream.foldLeft(Board(Nil))((currentBoard, key) => {
      val userInput: Char = key.toChar
      parse(currentBoard, userInput)
    })
  }

  def parse(board: Board, userInput: Char): Board ={

    board.print

    board
      .gravity
      .moveBlocks(userInput)
      .collision
      .gameOverCheck
      .fullLineCheck
      .spawn
      .tickOne
  }
}
