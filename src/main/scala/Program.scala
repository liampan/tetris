object Program {
  var speed = 100

  // Very JAVA-ry here...
  def main(args: Array[String]): Unit = {
    Printer.clear()
    SecretHamletService.pingWakeUp
    Printer.printTitle
    val name = scala.io.StdIn.readLine("Enter name: ")

    val con = new jline.console.ConsoleReader
    val is = con.getInput
    val nbis = new jline.internal.NonBlockingInputStream(is, true)
    val charStream = Stream.iterate(0)(_ => nbis.read(speed))


    charStream.foldLeft(Board(Nil, name = name))((currentBoard, key) => {
      val userInput = key.toChar
      parse(currentBoard, userInput)
    })
  }

  def parse(board: Board, userInput: Char): Board ={
    board.print

    speed = board.speed

    board
      .evaluateSpeed
      .gravity
      .moveBlocks(userInput)
      .collisionCheck
      .gameOverAction
      .fullLineCheck
      .spawn
      .tickOne
  }

}
