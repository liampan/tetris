import org.scalatest.{MustMatchers, WordSpec}
import Rotate._

class RotateSpec extends WordSpec with MustMatchers {

  "Rotate" must {

    val shapes: List[(String, List[Int], List[Int])] =
      List(
        (ShapeNames.lShape, List(3, 4, 14, 24), List(5, 15, 14, 13)),
        (ShapeNames.line,   List(4, 14, 24, 34), List(16, 15, 14, 13)),
        (ShapeNames.zShape, List(26, 37, 36, 47), List(37, 28, 36, 27)),
        (ShapeNames.sShape, List(13, 23, 24, 34), List(15, 14, 24, 23))
        //add L J T sqaure shapes
      )

    for ((shapeName, startIndexes, endIndexes) <- shapes){
      s"rotate a $shapeName 90 clockwise when no obstruction" in {

       val board = startIndexes.map(Tetromino(shapeName, "noColour", _))
        board.map(_.rotateState) mustEqual List(0, 0, 0, 0)

        rotate(board).map(_.rotateState) mustEqual List(1, 1, 1, 1)
        rotate(board).map(_.index) mustEqual endIndexes
      }
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