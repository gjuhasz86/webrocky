package roborock.mapparser

import java.nio.ByteBuffer
import java.nio.ByteOrder

import scala.annotation.tailrec

object ByteParser {

  def parse(types: DataType*)(input: Vector[Byte]): List[Int] = {
    @tailrec
    def loop(types: List[DataType], inBytes: Vector[Byte], acc: List[Int]): List[Int] = types match {
      case Nil => acc
      case h :: restTypes =>
        val (result, restBytes) = h.consume(inBytes)
        loop(restTypes, restBytes, result :: acc)
    }
    loop(types.toList, input, Nil).reverse
  }

  sealed trait DataType {
    def consume(input: Vector[Byte]): (Int, Vector[Byte])
  }
  case object S extends DataType {
    override def consume(input: Vector[Byte]): (Int, Vector[Byte]) =
      ByteBuffer.wrap(input.take(2).toArray).order(ByteOrder.LITTLE_ENDIAN).getShort.toInt -> input.drop(2)
  }
  case object I extends DataType {
    override def consume(input: Vector[Byte]): (Int, Vector[Byte]) =
      ByteBuffer.wrap(input.take(4).toArray).order(ByteOrder.LITTLE_ENDIAN).getInt -> input.drop(4)
  }
}