import org.scalatest.{MustMatchers, WordSpec}

class TetrominoSpec extends WordSpec with MustMatchers {

  "getIndex()" must {
    "return an index of a shape" in {
      val block = new Tetromino("dot", "red", 5)

      block.getIndex() mustEqual 5
    }
  }

  "moveLeft()" must {
    "change an index by -1" in {
      val block = new Tetromino("dot", "red", 5)
      block.moveLeft()

      block.getIndex() mustEqual 4
    }

    "not change the index off of the board" in{
      val block = new Tetromino("dot", "red", 0)
      block.moveLeft()

      block.getIndex() mustEqual 0
    }

    "not change the index off of the board on a different line" in{
      val block = new Tetromino("dot", "red", 10)
      block.moveLeft()

      block.getIndex() mustEqual 10
    }
  }

  "moveRight()" must {
    "change an index by +1" in {
      val block = new Tetromino("dot", "red", 5)
      block.moveRight()

      block.getIndex() mustEqual 6
    }

    "not change the index off of the board" in{
      val block = new Tetromino("dot", "red", 9)
      block.moveRight()

      block.getIndex() mustEqual 9
    }

    "not change the index off of the board on a different line" in{
      val block = new Tetromino("dot", "red", 19)
      block.moveRight()

      block.getIndex() mustEqual 19
    }
  }

  "fall()" must {
    "change an index width of the board[10]" in {
      val block = new Tetromino("dot", "red", 5)
      val boardWidth = 10
      block.fall(boardWidth)

      block.getIndex() mustEqual 15
    }
  }
}
