package roborock
import roborock.core.Pos

trait Converter[-A, +B] {
  def convert(a: A): B
}

object Converter {

  implicit class PosConverterOps[A](val self: A) extends AnyVal {
    def to[B](implicit c: Converter[A, B]): B = c.convert(self)
  }

  implicit def worldScreenPosConverter(
    implicit wi: Converter[Pos.World, Pos.Img], is: Converter[Pos.Img, ScreenPos]
  ): Converter[Pos.World, ScreenPos] =
    (wp: Pos.World) => is.convert(wi.convert(wp))

  implicit def screenWorldPosConverter(
    implicit iw: Converter[Pos.Img, Pos.World], si: Converter[ScreenPos, Pos.Img]
  ): Converter[ScreenPos, Pos.World] =
    (sp: ScreenPos) => iw.convert(si.convert(sp))

  trait WorldImgPosConverter extends Converter[Pos.World, Pos.Img] {
    def offset: Pos.Img
    override def convert(wp: Pos.World): Pos.Img =
      Pos.Img((wp.x / 50.0) - offset.x, (wp.y / 50.0) - offset.y)
  }

  trait ImgWorldPosConverter extends Converter[Pos.Img, Pos.World] {
    def offset: Pos.Img
    override def convert(ip: Pos.Img): Pos.World =
      Pos.World(Math.round((offset.x + ip.xx) * 50).toInt, Math.round((offset.y + ip.yy) * 50).toInt)
  }

  trait ScreenImgPosConverter extends Converter[ScreenPos, Pos.Img] {
    def scale: Double
    override def convert(sp: ScreenPos): Pos.Img =
      Pos.Img(sp.y / scale, sp.x / scale)
  }

  trait ImgScreenPosConverter extends Converter[Pos.Img, ScreenPos] {
    def scale: Double
    override def convert(ip: Pos.Img): ScreenPos =
      ScreenPos(Math.round(ip.yy * scale).toInt, Math.round(ip.xx * scale).toInt)
  }

}