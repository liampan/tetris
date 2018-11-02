import org.scalatest.{MustMatchers, WordSpec}
import Rotate._

class RotateSpec extends WordSpec with MustMatchers {

  "Rotate" must {

    "rotate a L shape 90 clockwise with no obstructions with start = List(3, 4, 14, 24)" in {
        val colour = "red"
        val name = ShapeNames.lShape
        val indexes = List(3, 4, 14, 24)

      val board = indexes.map(Tetromino(name, colour, _))
      board.map(_.rotateState) mustEqual List(0, 0, 0, 0)




      rotate(board).map(_.rotateState) mustEqual List(1, 1, 1, 1)
      rotate(board).map(_.index) mustEqual List(5, 15, 14, 13)
    }

    "rotate a L again with no obstruction" in {
      val colour = "red"
      val name = ShapeNames.lShape
      val indexes = List(5, 15, 14, 13)

      val board = indexes.map(Tetromino(name, colour, _).rotated)


      board.map(_.rotateState) mustEqual List(1, 1, 1, 1)



      rotate(board).map(_.rotateState) mustEqual List(2, 2, 2, 2)
      rotate(board).map(_.index) mustEqual List(25, 24, 14, 4)
    }

  }
}