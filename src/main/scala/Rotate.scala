
import ShapeNames._

object Rotate {

  def rotate(board: List[Tetromino]): Unit = {
    val userControlled = board.filter(_.userCanControl)


    if (userControlled.nonEmpty) {
      userControlled.head.getShape match {
        case `lShape` => rotateL(board)
        case `tee`    => rotateT(board)
        case `jShape` => rotateJ(board)
        case `line`   => rotateLine(board)
        case `zShape` => rotateZ(board)
        case `sShape` => rotateS(board)
        case _ => // do nothing
      }
    }
  }

  private def rotateS(board: List[Tetromino]): Unit = {
    val tets = board.filter(_.userCanControl)
    val oneShape = tets.length == 4
    val canRotate = true // collision for when rotated.


    if (oneShape && canRotate) {
      val moves = tets.head.rotateState match {
        case 0 => List(2, -9, 0, -11)
        case 1 => List(20, 11, 0, -9)
        case 2 => List(-2, 9, 0, 11)
        case 3 => List(-20, -11, 0, 9)
      }
      tets.zip(moves).foreach { case (tet: Tetromino, moveBy: Int) => tet.setIndex(tet.getIndex + moveBy) }
      tets.foreach(t => if (t.rotateState < 3) t.rotateState = t.rotateState + 1 else t.rotateState = 0)
    }
  }

  private def rotateZ(board: List[Tetromino]): Unit = {
    val tets = board.filter(_.userCanControl)
    val oneShape = tets.length == 4
    val canRotate = true // collision for when rotated.


    if (oneShape && canRotate) {
      val moves = tets.head.rotateState match {
        case 0 => List(11, -9, 0, -20)
        case 1 => List(9, 11, 0, 2)
        case 2 => List(-11, 9, 0, 20)
        case 3 => List(-9, -11, 0, -2)
      }
      tets.zip(moves).foreach { case (tet: Tetromino, moveBy: Int) => tet.setIndex(tet.getIndex + moveBy) }
      tets.foreach(t => if (t.rotateState < 3) t.rotateState = t.rotateState + 1 else t.rotateState = 0)
    }
  }

  private def rotateLine(board: List[Tetromino]): Unit = {
    val tets = board.filter(_.userCanControl)
    val oneShape = tets.length == 4
    val canRotate = true // collision for when rotated.


    if (oneShape && canRotate) {
      val moves = tets.head.rotateState match {
        case 0 => List(12, 1, -10, -21)
        case 1 => List(19, 10, 1, -8)
        case 2 => List(-12, -1, 10, 21)
        case 3 => List(-19, -10, -1, 8)
      }
      tets.zip(moves).foreach { case (tet: Tetromino, moveBy: Int) => tet.setIndex(tet.getIndex + moveBy) }
      tets.foreach(t => if (t.rotateState < 3) t.rotateState = t.rotateState + 1 else t.rotateState = 0)
    }
  }

  private def rotateJ(board: List[Tetromino]): Unit = {
    val tets = board.filter(_.userCanControl)
    val oneShape = tets.length == 4
    val canRotate = true // collision for when rotated.


    if (oneShape && canRotate) {
      val moves = tets.head.rotateState match {
        case 0 => List(11, 20, 0, -11)
        case 1 => List(9, -2, 0, -9)
        case 2 => List(-11, -20, 0, 11)
        case 3 => List(-9, 2, 0, 9)
      }
      tets.zip(moves).foreach { case (tet: Tetromino, moveBy: Int) => tet.setIndex(tet.getIndex + moveBy) }
      tets.foreach(t => if (t.rotateState < 3) t.rotateState = t.rotateState + 1 else t.rotateState = 0)
    }
  }

  private def rotateT(board: List[Tetromino]): Unit = {
    val tets = board.filter(_.userCanControl)
    val oneShape = tets.length == 4
    val canRotate = true // collision for when rotated.


    if (oneShape && canRotate) {
      val moves = tets.head.rotateState match {
        case 0 => List(11, -9, 0, 9)
        case 1 => List(9, 11, 0, -11)
        case 2 => List(-11, 9, 0, -9)
        case 3 => List(-9, -11, 0, 11)
      }
      tets.zip(moves).foreach { case (tet: Tetromino, moveBy: Int) => tet.setIndex(tet.getIndex + moveBy) }
      tets.foreach(t => if (t.rotateState < 3) t.rotateState = t.rotateState + 1 else t.rotateState = 0)
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
