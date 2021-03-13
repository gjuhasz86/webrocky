package roborock.core

object Pos {
  case class World(x: Int, y: Int)
  case class Img(xx: Double, yy: Double) {
    val (x, y) = (xx.toInt, yy.toInt)
  }
}