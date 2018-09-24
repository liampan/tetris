import org.scalatest.{MustMatchers, WordSpec}

class TetrominoSpec extends WordSpec with MustMatchers {

  "getIndex" must {
    "return an index of a shape" in {
      val block = new Tetromino("dot", "red", 5)

      block.getIndex mustEqual 5
    }
  }

  "moveLeft" must {
    "change an index by -1" in {
      val block = new Tetromino("dot", "red", 5)
      block.moveLeft(List(block))

      block.getIndex mustEqual 4
    }

    "not change the index off of the board" in{
      val block = new Tetromino("dot", "red", 0)
      reader.collision(List(block))
      block.moveLeft(List(block))

      block.getIndex mustEqual 0
    }

    "not change the index off of the board on a different line" in{
      val block = new Tetromino("dot", "red", 10)
      reader.collision(List(block))
      block.moveLeft(List(block))

      block.getIndex mustEqual 10
    }
  }

  "moveRight" must {
    "change an index by +1" in {
      val block = new Tetromino("dot", "red", 5)
      block.moveRight(List(block))

      block.getIndex mustEqual 6
    }

    "not change the index off of the board" in{
      val block = new Tetromino("dot", "red", 9)
      reader.collision(List(block))
      block.moveRight(List(block))

      block.getIndex mustEqual 9
    }

    "not change the index off of the board on a different line" in{
      val block = new Tetromino("dot", "red", 19)
      reader.collision(List(block))
      block.moveRight(List(block))

      block.getIndex mustEqual 19
    }
  }

  "fall" must {
    "change an index width of the board[10]" in {
      val block = new Tetromino("dot", "red", 5)
      val boardWidth = 10
      block.fall(boardWidth)

      block.getIndex mustEqual 15
    }

    "not fall off the bottom of the screen" in {
      val block = new Tetromino("dot", "red", 145)
      val boardWidth = 10
      block.fall(boardWidth)

      block.getIndex mustEqual 145
    }

    "toggle a shapes movement ability" in {
      val block = new Tetromino("dot", "red", 145)
      val boardWidth = 10
      block.fall(boardWidth)
      block.moveRight(List(block))
      block.moveRight(List(block))

      block.getIndex mustEqual 145
    }
  }

}
