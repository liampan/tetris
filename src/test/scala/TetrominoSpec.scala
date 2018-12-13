import org.scalatest.{MustMatchers, WordSpec}

class TetrominoSpec extends WordSpec with MustMatchers {

  "getIndex" must {
    "return an index of a shape" in {
      val block = Tetromino("dot", "red", 5)

      block.index mustEqual 5
    }
  }

  "getLook" must {

    val colours = List(
      ("red", Console.RED),
      ("blue", Console.BLUE),
      ("cyan", Console.CYAN),
      ("green", Console.GREEN),
      ("yellow", Console.YELLOW),
      ("magenta", Console.MAGENTA))

    for ((colour, console) <- colours) {
      s"return a $colour coloured block" in {
        val block = Tetromino("dot", colour, 5)

        block.getLook mustEqual s"$console■${Console.RESET}"
      }
    }
  }

  "move left with input 'a' " must {
    "change an index by -1" in {
      val board = Board(List(Tetromino("dot", "red", 5)), Nil).collisionCheck

      board.moveBlocks('a').blocks.head.index mustEqual 4
    }

    "not change the index off of the board" in {
      val board = Board(List(Tetromino("dot", "red", 0)), Nil).collisionCheck

      board.moveBlocks('a').blocks.head.index mustEqual 0
    }

    "not change the index off of the board on a different line" in {
      val board = Board(List(Tetromino("dot", "red", 10)), Nil).collisionCheck

      board.moveBlocks('a').blocks.head.index mustEqual 10
    }
  }

  "move right with input 'd'  " must {
    "change an index by +1" in {
      val board = Board(List(Tetromino("dot", "red", 5)), Nil).collisionCheck

      board.moveBlocks('d').blocks.head.index mustEqual 6
    }

    "not change the index off of the board" in {
      val board = Board(List(Tetromino("dot", "red", 9)), Nil).collisionCheck

      board.moveBlocks('d').blocks.head.index mustEqual 9
    }

    "not change the index off of the board on a different line" in {
      val board = Board(List(Tetromino("dot", "red", 19)), Nil).collisionCheck

      board.moveBlocks('d').blocks.head.index mustEqual 19
    }
  }

  "fall" must {
    "change an index width of the board[10]" in {
      val block = Tetromino("dot", "red", 5)
      val boardWidth = 10
      val newblock = block.fall(boardWidth)

      newblock.index mustEqual 15
      newblock.canFall mustEqual true
    }

    "not fall off the bottom of the screen" in {
      val block = Tetromino("dot", "red", 145)
      val boardWidth = 10
      val newblock = block.fall(boardWidth)

      newblock.index mustEqual 145
      newblock.canFall mustEqual false
    }

    "toggle a shapes movement ability" in {
      val block = Tetromino("dot", "red", 145)
      val boardWidth = 10
      val newblock = block.fall(boardWidth)


      newblock.index mustEqual 145
      newblock.canFall mustEqual false
      newblock.userCanControl mustEqual false
    }
  }

}
