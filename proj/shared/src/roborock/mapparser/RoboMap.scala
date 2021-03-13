package roborock.mapparser

import roborock.core.Pos

import scala.annotation.tailrec

case class Zone(topLeft: Pos.World, bottomRight: Pos.World)

case class RoboMap(blocks: List[RoboMapBlock]) {

  val Some(imageBlock) = blocks.collectFirst { case b: ImageBlock => b }
  val Some(roboPos) = blocks.collectFirst { case b: RobotPosBlock => b }
  val Some(vacPath) = blocks.collectFirst { case b: VacuumPathBlock => b }
  val predPath = blocks.collectFirst { case b: PredictedPathBlock => b }
  val cleanedZones = blocks.collectFirst { case b: CleanedZonesBlock => b }
  val noGoZones = blocks.collectFirst { case b: NoGoZonesBlock => b }
  val noMopZones = blocks.collectFirst { case b: NoMopZonesBlock => b }

  def pointAt(p: Pos.Img): Byte = pointAt(p.x, p.y)
  def pointAt(x: Int, y: Int): Byte = imageBlock.image(y * imageBlock.width + x)

}

sealed trait RoboMapBlock {def blockLength: Int}
case class ImageBlock(offset: Pos.Img, height: Int, width: Int, blockLength: Int)(val image: Vector[Byte]) extends RoboMapBlock
case class VacuumPathBlock(pointCount: Int, blockLength: Int)(val points: List[Pos.World]) extends RoboMapBlock
case class GotoPathBlock(pointCount: Int, blockLength: Int)(val points: List[Pos.World]) extends RoboMapBlock
case class PredictedPathBlock(pointCount: Int, blockLength: Int)(val points: List[Pos.World]) extends RoboMapBlock
case class RobotPosBlock(pos: Pos.World, angle: Int, blockLength: Int) extends RoboMapBlock
case class CleanedZonesBlock(zoneCount: Int, blockLength: Int)(val zones: List[Zone]) extends RoboMapBlock
case class NoGoZonesBlock(zoneCount: Int, blockLength: Int)(val zones: List[Zone]) extends RoboMapBlock
case class NoMopZonesBlock(zoneCount: Int, blockLength: Int)(val zones: List[Zone]) extends RoboMapBlock
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
        case 3 | 4 | 5 => parseVacuumPath(data)
        case 6 => parseCleanedZones(data)
        case 9 | 12 => parseZones(data)
        case 8 => parseRobotPos(data)
        case x =>
          println(s"Skipping block type: $x")
          skip(data)
      }
    block
  }

  def parseImage(data: Vector[Byte]): ImageBlock = {
    import ByteParser._
    val List(_, hLen, dLen, _, yOffs, xOffs, height, width) = ByteParser.parse(S, S, I, I, I, I, I, I)(data)
    ImageBlock(Pos.Img(xOffs, yOffs), height, width, hLen + dLen)(data.slice(hLen, hLen + dLen))
  }

  def parseVacuumPath(data: Vector[Byte]): RoboMapBlock = {
    import ByteParser._
    val List(id, hLen, dLen, pCount, pSize, pAngle) = ByteParser.parse(S, S, I, I, I, I)(data)
    val points =
      data.slice(hLen, hLen + dLen)
        .grouped(4)
        .map(ByteParser.parse(S, S)(_))
        .map { case List(x, y) => Pos.World(x, y) }
        .toList
    id match {
      case 3 => VacuumPathBlock(pCount, hLen + dLen)(points)
      case 4 => GotoPathBlock(pCount, hLen + dLen)(points)
      case 5 => PredictedPathBlock(pCount, hLen + dLen)(points)
    }
  }

  def parseRobotPos(data: Vector[Byte]): RobotPosBlock = {
    import ByteParser._
    val List(_, hLen, dLen, x, y, angle) = ByteParser.parse(S, S, I, I, I, I)(data)
    RobotPosBlock(Pos.World(x, y), angle, hLen + dLen)
  }


  def parseCleanedZones(data: Vector[Byte]): RoboMapBlock = {
    import ByteParser._
    val List(_, hLen, dLen, zoneCount) = ByteParser.parse(S, S, I, S)(data)
    val zones =
      data.slice(hLen, hLen + dLen)
        .grouped(8)
        .map(ByteParser.parse(S, S, S, S)(_))
        .map { case List(x1, y1, x2, y2) => Zone(Pos.World(x1, y1), Pos.World(x2, y2)) }
        .toList

    CleanedZonesBlock(zoneCount, hLen + dLen)(zones)
  }

  def parseZones(data: Vector[Byte]): RoboMapBlock = {
    import ByteParser._
    val List(id, hLen, dLen, zoneCount) = ByteParser.parse(S, S, I, S)(data)
    val zones =
      data.slice(hLen, hLen + dLen)
        .grouped(16)
        .map(ByteParser.parse(S, S, S, S, S, S, S, S)(_))
        .map { case List(x1, y2, _, _, x2, y1, _, _) => Zone(Pos.World(x1, y1), Pos.World(x2, y2)) }
        .toList

    id match {
      case 9 => NoGoZonesBlock(zoneCount, hLen + dLen)(zones)
      case 12 => NoMopZonesBlock(zoneCount, hLen + dLen)(zones)
    }
  }

  def skip(data: Vector[Byte]): UnknownBlock = {
    import ByteParser._
    val List(_, hLen, dLen) = ByteParser.parse(S, S, I)(data)
    UnknownBlock(hLen + dLen)
  }
}
