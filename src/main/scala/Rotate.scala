object Rotate {

  def rotate(board: List[Tetromino]): Unit = {
    val userControlled = board.filter(_.userCanControl)

    userControlled.map(_.getShape).head match { //not nice
      case "Lshape" => rotateL(board)
      case _ => // do nothing
    }
  }

  def rotateL(board: List[Tetromino]): Unit = {
    val tets = board.filter(_.userCanControl)
    val oneShape = tets.length == 4
    val canRotate = true // collision for when rotated.



    if (oneShape && canRotate){
      tets.head.rotateState match {
        case 0 =>{
          val moves = List(2, 11, 0, -11)
          tets.zip(moves).foreach{case (tet: Tetromino, moveBy: Int) => tet.setIndex(tet.getIndex + moveBy)}
        }
        case 1 =>{
          val moves = List(20, 9, 0, -9)
          tets.zip(moves).foreach{case (tet: Tetromino, moveBy: Int) => tet.setIndex(tet.getIndex + moveBy)}
        }
        case 2 =>{
          val moves = List(-2, -11, 0, 11)
          tets.zip(moves).foreach{case (tet: Tetromino, moveBy: Int) => tet.setIndex(tet.getIndex + moveBy)}
        }
        case 3 =>{
          val moves = List(-20, -9, 0, 9)
          tets.zip(moves).foreach{case (tet: Tetromino, moveBy: Int) => tet.setIndex(tet.getIndex + moveBy)}
        }
      }
      tets.foreach(t => if(t.rotateState < 3) t.rotateState = t.rotateState+1 else t.rotateState = 0)
    }

  }




}
