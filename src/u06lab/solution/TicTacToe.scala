package u06lab.solution

object TicTacToe extends App {
  sealed trait Player{
    def other: Player = this match {case X => O; case _ => X}
    override def toString: String = this match {case X => "X"; case _ => "O"}
  }
  case object X extends Player
  case object O extends Player

  case class Mark(x: Int, y: Int, player: Player)
  type Board = List[Mark]
  type Game = List[Board]

  // versione usando board.find()
  // def find(board: Board, x: Int, y: Int): Option[Player] = board find {m=>m.x == x && m.y==y} map (_.player)

  def find(board: Board, x: Int, y: Int): Option[Player] = board match {
    case Mark(`x`,`y`,p) :: _ => Some(p)
    case _ :: t => find(t,x,y)
    case _ => None
  }

  def placeAnyMark(board: Board, player: Player): Seq[Board] = {
    if (gameNotEnded(board)) {
      var plays:List[Board] = List[Board]()
      for (x <- 2 to 0 by -1;y <- 2 to 0 by -1;if find(board,x,y).isEmpty) plays = (Mark(x,y,player) :: board) :: plays
      plays
    } else
      List(Nil)
  }

  def computeAnyGame(player: Player, moves: Int): Stream[Game] = moves match {
    case 0 => Stream(List(Nil))
    case _ => for {
      game <- computeAnyGame(player.other,moves - 1)
      play <- placeAnyMark(game.head,player.other)
    } yield play :: game
  }
  def computeAnyGameBetter(player: Player, moves: Int): Stream[Game] = moves match {
    case 0 => Stream(List(Nil))
    case _ => for {
      game <- computeAnyGameBetter(player.other,moves - 1)
      play <- placeAnyMark(game.head,player.other)
    } yield if (play==Nil) game else play :: game
  }

  def gameNotEnded(board: Board):Boolean = board forall (m => m match {
    case Mark(0,0,p) => (!find(board, 1, 1).contains(p) || !find(board, 2, 2).contains(p)) && (!find(board, 0, 1).contains(p) || !find(board, 0, 2).contains(p)) && (!find(board, 1, 0).contains(p) || !find(board, 2, 0).contains(p))
    case Mark(2,0,p) => (!find(board, 1, 1).contains(p) || !find(board, 0, 2).contains(p)) && (!find(board, 2, 1).contains(p) || !find(board, 2, 2).contains(p))
    case Mark(1,0,p) => !find(board, 1, 1).contains(p) || !find(board, 1, 2).contains(p)
    case Mark(0,y,p) => !find(board, 1, y).contains(p) || !find(board, 2, y).contains(p)
    case _ => true
  })
  
  def printBoards(game: Seq[Board]): Unit =
    for (y <- 0 to 2; board <- game.reverse; x <- 0 to 2) {
      print(find(board, x, y) map (_.toString) getOrElse ("."))
      if (x == 2) { print(" "); if (board == game.head) println()}
    }

  // Exercise 1: implement find such that..
  println(find(List(Mark(0,0,X)),0,0)) // Some(X)
  println(find(List(Mark(0,0,X),Mark(0,1,O),Mark(0,2,X)),0,1)) // Some(O)
  println(find(List(Mark(0,0,X),Mark(0,1,O),Mark(0,2,X)),1,1)) // None

  // Exercise 2: implement placeAnyMark such that..
  printBoards(placeAnyMark(List(),X))
  //... ... ..X ... ... .X. ... ... X..
  //... ..X ... ... .X. ... ... X.. ...
  //..X ... ... .X. ... ... X.. ... ...
  //printBoards(placeAnyMark(List(Mark(0,0,O)),X))
  //O.. O.. O.X O.. O.. OX. O.. O..
  //... ..X ... ... .X. ... ... X..
  //..X ... ... .X. ... ... X.. ...

  // Exercise 3 (ADVANCED!): implement computeAnyGame such that..
  computeAnyGameBetter(O, 9) foreach {g => if (!gameNotEnded(g.head))  { if (g.length<10) { println(g.length+" PASSI VINTO!");printBoards(g); println()} else {println("10 PASSI VINTO");printBoards(g); println()} } else {println("NON VINTO");}}
  //... X.. X.. X.. XO.
  //... ... O.. O.. O..
  //... ... ... X.. X..
  //              ... computes many such games (they should be 9*8*7*6 ~ 3000).. also, e.g.:
  //
  //... ... .O. XO. XOO
  //... ... ... ... ...
  //... .X. .X. .X. .X.

  // Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
}
