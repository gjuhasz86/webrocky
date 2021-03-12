package roborock.mapparser

import scala.annotation.tailrec

case class Pos(x: Int, y: Int)
case class RoboMap(blocks: List[RoboMapBlock]) {

  val Some(imageBlock) = blocks.collectFirst { case b: ImageBlock => b }
  val Some(roboPos) = blocks.collectFirst { case b: RobotPosBlock => b }
  val Some(vacPath) = blocks.collectFirst { case b: VacuumPathBlock => b }

  def pointAt(p: Pos): Byte = pointAt(p.x, p.y)
  def pointAt(x: Int, y: Int): Byte = imageBlock.image(y * imageBlock.width + x)

}

sealed trait RoboMapBlock {def blockLength: Int}
case class ImageBlock(offset: Pos, height: Int, width: Int, blockLength: Int)(val image: Vector[Byte]) extends RoboMapBlock
case class VacuumPathBlock(blockLength: Int)(val points: List[Pos]) extends RoboMapBlock
case class RobotPosBlock(pos: Pos, angle: Int, blockLength: Int) extends RoboMapBlock
case class UnknownBlock(blockLength: Int) extends RoboMapBlock

object RoboMapBlock {

  def parseMany(data: Vector[Byte]): List[RoboMapBlock] = {
    @tailrec
    def loop(input: Vector[Byte], acc: List[RoboMapBlock]): List[RoboMapBlock] = {
      val block = parse(input)
      if (input.size > block.blockLength)
        loop(input.drop(block.blockLength), block :: acc)
      else
        acc
    }
    loop(data, Nil).reverse
  }

  def parse(data: Vector[Byte]): RoboMapBlock = {
    import ByteParser._
    val List(blockType) = ByteParser.parse(S)(data)
    val block =
      blockType match {
        case 2 => parseImage(data)
        case 3 => parseVacuumPath(data)
        case 8 => parseRobotPos(data)
        case x =>
          //          println(s"Skipping block type: $x")
          skip(data)
      }
    block
  }

  def parseImage(data: Vector[Byte]): ImageBlock = {
    import ByteParser._
    val List(_, hLen, dLen, _, yOffs, xOffs, height, width) = ByteParser.parse(S, S, I, I, I, I, I, I)(data)
    ImageBlock(Pos(xOffs, yOffs), height, width, hLen + dLen)(data.slice(hLen, hLen + dLen))
  }

  def parseVacuumPath(data: Vector[Byte]): VacuumPathBlock = {
    import ByteParser._
    val List(_, hLen, dLen, pCount, pSize, pAngle) = ByteParser.parse(S, S, I, I, I, I)(data)
    val points =
      data.slice(hLen, hLen + dLen)
        .grouped(4)
        .map(ByteParser.parse(S, S)(_))
        .map { case List(x, y) => Pos(x, y) }
        .toList
    VacuumPathBlock(hLen + dLen)(points)
  }

  def parseRobotPos(data: Vector[Byte]): RobotPosBlock = {
    import ByteParser._
    val List(_, hLen, dLen, x, y, angle) = ByteParser.parse(S, S, I, I, I, I)(data)
    RobotPosBlock(Pos(x, y), angle, hLen + dLen)
  }

  def skip(data: Vector[Byte]): UnknownBlock = {
    import ByteParser._
    val List(_, hLen, dLen) = ByteParser.parse(S, S, I)(data)
    UnknownBlock(hLen + dLen)
  }
}
