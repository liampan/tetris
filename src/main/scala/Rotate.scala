
import ShapeNames._

object Rotate {

  def rotate(board: List[Tetromino]): Unit = {
    val userControlled = board.filter(_.userCanControl)

    if (userControlled.nonEmpty) {
      userControlled.map(_.getShape).head match {
        case `lShape` => rotateL(board)
        case _ => // do nothing
      }
    }
  }

  private def rotateL(board: List[Tetromino]): Unit = {
    val tets = board.filter(_.userCanControl)
    val oneShape = tets.length == 4
    val canRotate = true // collision for when rotated.


    if (oneShape && canRotate) {
      val moves = tets.head.rotateState match {
        case 0 => List(2, 11, 0, -11)
        case 1 => List(20, 9, 0, -9)
        case 2 => List(-2, -11, 0, 11)
        case 3 => List(-20, -9, 0, 9)
      }
      tets.zip(moves).foreach { case (tet: Tetromino, moveBy: Int) => tet.setIndex(tet.getIndex + moveBy) }
      tets.foreach(t => if (t.rotateState < 3) t.rotateState = t.rotateState + 1 else t.rotateState = 0)
    }

  }


}
