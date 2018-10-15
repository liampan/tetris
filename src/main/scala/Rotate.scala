
import ShapeNames._

object Rotate {

  def rotate(board: List[Tetromino]): Unit = {
    val userControlled = board.filter(_.userCanControl)
    val oneShape = userControlled.length == 4

    if (userControlled.nonEmpty && oneShape) {
      val canRotate = true // collision for when rotated.

      if (canRotate) {
        val moves : List[Int] = userControlled.head.getShape match {
          case `lShape` => rotateL(userControlled)
          case `tee`    => rotateT(userControlled)
          case `jShape` => rotateJ(userControlled)
          case `zShape` => rotateZ(userControlled)
          case `sShape` => rotateS(userControlled)
          case `line`   => rotateLine(userControlled)
        }

        userControlled.zip(moves).foreach { case (tet: Tetromino, moveBy: Int) => tet.setIndex(tet.getIndex + moveBy) }
        userControlled.foreach(t => if (t.rotateState < 3) t.rotateState = t.rotateState + 1 else t.rotateState = 0)
      }
    }
  }

  private def rotateS(tets: List[Tetromino]): List[Int] = {
    tets.head.rotateState match {
        case 0 => List(2, -9, 0, -11)
        case 1 => List(20, 11, 0, -9)
        case 2 => List(-2, 9, 0, 11)
        case 3 => List(-20, -11, 0, 9)
    }
  }

  private def rotateZ(tets: List[Tetromino]): List[Int] = {
    tets.head.rotateState match {
        case 0 => List(11, -9, 0, -20)
        case 1 => List(9, 11, 0, 2)
        case 2 => List(-11, 9, 0, 20)
        case 3 => List(-9, -11, 0, -2)
      }
  }

  private def rotateLine(tets: List[Tetromino]): List[Int] = {
    tets.head.rotateState match {
        case 0 => List(12, 1, -10, -21)
        case 1 => List(19, 10, 1, -8)
        case 2 => List(-12, -1, 10, 21)
        case 3 => List(-19, -10, -1, 8)
      }
  }

  private def rotateJ(tets: List[Tetromino]): List[Int] = {
    tets.head.rotateState match {
        case 0 => List(11, 20, 0, -11)
        case 1 => List(9, -2, 0, -9)
        case 2 => List(-11, -20, 0, 11)
        case 3 => List(-9, 2, 0, 9)
      }
  }

  private def rotateT(tets: List[Tetromino]): List[Int] = {
    tets.head.rotateState match {
        case 0 => List(11, -9, 0, 9)
        case 1 => List(9, 11, 0, -11)
        case 2 => List(-11, 9, 0, -9)
        case 3 => List(-9, -11, 0, 11)
      }
  }

  private def rotateL(tets: List[Tetromino]): List[Int] = {
    tets.head.rotateState match {
        case 0 => List(2, 11, 0, -11)
        case 1 => List(20, 9, 0, -9)
        case 2 => List(-2, -11, 0, 11)
        case 3 => List(-20, -9, 0, 9)
      }
  }

}
