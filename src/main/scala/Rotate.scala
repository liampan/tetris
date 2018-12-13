
import ShapeNames._

object Rotate {

  def rotate(implicit blocks: List[Tetromino]): List[Tetromino] = {
    val oneShape = blocks.count(_.userCanControl) == 4
    lazy val canRotate = doesNotPhase && doesNotGoOffBoard

    if (oneShape ) {
      if(canRotate) {
        val userControlled: List[Tetromino] = blocks.filter(_.userCanControl)
        val restOfBlocks: List[Tetromino] = blocks.filterNot(_.userCanControl)

        val moves: List[Int] = userControlled.head.shape match {
          case `lShape` => rotateL(userControlled)
          case `tee`    => rotateT(userControlled)
          case `jShape` => rotateJ(userControlled)
          case `zShape` => rotateZ(userControlled)
          case `sShape` => rotateS(userControlled)
          case `line`   => rotateLine(userControlled)
          case `square` => List(0, 0, 0, 0)
        }

        val rotatedBlocks = userControlled.zip(moves).map {
          case (tet: Tetromino, moveBy: Int) =>
            tet.copy(index = tet.index + moveBy).rotated
        }
        restOfBlocks ++ rotatedBlocks
      }
      else blocks
    }
    else blocks
  }

  private def doesNotGoOffBoard(implicit blocks: List[Tetromino]): Boolean = {
    val movingTets = blocks.filter(t => t.userCanControl)
    val tetsIndexed = movingTets.map(_.index)
    movingTets.head.shape match {
      case `line` =>{
        movingTets.head.rotateState match {
          case 0 =>
            val onEdge = tetsIndexed.map(_%10).exists(i => i == 8 | i ==  9 | i == 0)
            !onEdge
          case 2 =>
            val onEdge = tetsIndexed.map(_%10).exists(i => i == 8 | i ==  9 | i == 0 | i == 1)
            !onEdge
          case _ => true
        }
      }
      case _ =>
        val onEdge = tetsIndexed.map(_%10).contains(0 | 9)
        !onEdge
    }
  }

  private def doesNotPhase(implicit blocks: List[Tetromino]): Boolean = {
    blocks.size == futureRotate(blocks).map(_.index).toSet.size
  }

  private def futureRotate(blocks: List[Tetromino]): List[Tetromino] = {
    val userControlled: List[Tetromino] = blocks.filter(_.userCanControl)
    val restOfBlocks: List[Tetromino] = blocks.filterNot(_.userCanControl)
    val oneShape = userControlled.length == 4

    if (userControlled.nonEmpty && oneShape) {

      val moves: List[Int] = userControlled.head.shape match {
        case `lShape` => rotateL(userControlled)
        case `tee` => rotateT(userControlled)
        case `jShape` => rotateJ(userControlled)
        case `zShape` => rotateZ(userControlled)
        case `sShape` => rotateS(userControlled)
        case `line` => rotateLine(userControlled)
        case `square` => List(0, 0, 0, 0)
      }

      val rotatedBlocks = userControlled.zip(moves).map {
        case (tet: Tetromino, moveBy: Int) =>
          tet.copy(index = tet.index + moveBy).rotated
      }
      restOfBlocks ++ rotatedBlocks
    }
    else blocks
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
