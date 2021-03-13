package roborock
import java.lang.Math.PI

import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import org.scalajs.dom
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.ext.Ajax
import org.scalajs.dom.html
import roborock.core.MiioMsg
import roborock.core.Pos
import roborock.mapparser.RoboMap
import roborock.mapparser.RoboMapBlock

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

case class ScreenPos(x: Int, y: Int)

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

object Main {

  var allData: Array[Byte] = Array()
  var group: Int = 344
  var roboMap: RoboMap = _
  var scale = 4.0

  import Converter._

  implicit val iwConv: ImgWorldPosConverter =
    new ImgWorldPosConverter {override def offset: Pos.Img = roboMap.imageBlock.offset}

  implicit val wiConv: WorldImgPosConverter =
    new WorldImgPosConverter {override def offset: Pos.Img = roboMap.imageBlock.offset}

  implicit val isConv: ImgScreenPosConverter =
    new ImgScreenPosConverter {override def scale: Double = Main.scale}

  implicit val siConv: ScreenImgPosConverter =
    new ScreenImgPosConverter {override def scale: Double = Main.scale}

  @JSExportTopLevel("main")
  def main(canvas: html.Canvas): Unit = {
    val renderCtx: CanvasRenderingContext2D =
      canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    renderCtx.canvas.width = dom.window.innerWidth.toInt
    renderCtx.canvas.height = dom.window.innerHeight.toInt

    def updateMap(): Unit = {
      Ajax.get("api/map").foreach { xhr =>
        if (xhr.status == 200) {
          val Right(data) = decode[Vector[Byte]](xhr.responseText)
          allData = data.toArray
          roboMap = RoboMap(RoboMapBlock.parseMany(data.drop(20)))
          println(s"[${js.Date.now()}] Map updated $roboMap")
          dom.window.requestAnimationFrame(updateScreen)
        } else {
          println("err")
        }
      }
    }

    def sendCmd(msg: MiioMsg): Unit = {
      Ajax.post("api/rawcommand", msg.asJson.noSpaces, headers = Map("Content-Type" -> "application/json"))
    }

    def updateScreen(timeStamp: Double): Unit =
      if (roboMap != null) {
        renderCtx.canvas.width = (roboMap.imageBlock.width * scale).toInt + 1
        renderCtx.canvas.height = (roboMap.imageBlock.height * scale).toInt + 1
        renderCtx.clearRect(0, 0, canvas.width, canvas.height)
        process(renderCtx)
      }

    dom.window.oncontextmenu = (e: dom.MouseEvent) => {e.preventDefault()}

    canvas.onmousedown = (e: dom.MouseEvent) => {
      val sPos = ScreenPos(e.pageX.toInt, e.pageY.toInt)
      val wPos = sPos.to[Pos.World]

      println(s"$sPos ${sPos.to[Pos.Img]} ${sPos.to[Pos.Img].to[Pos.World]} ${sPos.to[Pos.World].to[Pos.Img]}")
      e.button match {
        case 0 if roboMap != null =>
          println(wPos)
        case 2 if roboMap != null =>
          val msg = MiioMsg.of("app_goto_target", s"[${wPos.x},${wPos.y}]")
          println(msg)
          sendCmd(msg)
        case _ =>
      }
    }

    dom.document.onkeyup = (e: dom.KeyboardEvent) => {
      e.key match {
        case "+" => scale = scale + 1
        case "-" => scale = scale - 1
        case _ =>
      }
      dom.window.requestAnimationFrame(updateScreen)
    }

    updateMap()
    dom.window.setInterval(() => updateMap(), 10000)
  }

  def process(ctx: CanvasRenderingContext2D): Unit = {
    val colors = Array("#e6194b", "#3cb44b", "#ffe119", "#4363d8", "#f58231", "#911eb4", "#46f0f0", "#f032e6", "#bcf60c", "#fabebe", "#008080", "#e6beff", "#9a6324", "#fffac8", "#800000", "#aaffc3", "#808000", "#ffd8b1", "#000075", "#808080", "#ffffff", "#000000")
    val colorMap = roboMap.imageBlock.image.distinct.filter(x => (x & 3) != 1).sorted.zip(colors).toMap
    ctx.beginPath()
    ctx.lineWidth = 1
    for (x <- 0 until roboMap.imageBlock.width; y <- 0 until roboMap.imageBlock.height) {
      val sPos = Pos.Img(x, y).to[ScreenPos]
      roboMap.pointAt(x, y) match {
        case 0 =>
          ctx.fillStyle = "white"
        case c if (c & 3) == 0 =>
          ctx.fillStyle = colorMap(c)
        case c if (c & 3) == 1 =>
          ctx.fillStyle = "black"
        case c if (c & 3) == 3 =>
          ctx.fillStyle = colorMap(c)
        case c =>
          println(c)
      }
      ctx.fillRect(sPos.x - (scale / 2), sPos.y - (scale / 2), scale, scale)
    }

    ctx.beginPath()
    ctx.lineWidth = 3.4 * scale
    ctx.strokeStyle = "red"
    ctx.lineJoin = "round"
    roboMap.vacPath.points match {
      case Nil =>
      case head :: tail =>
        val sp0 = head.to[ScreenPos]
        ctx.moveTo(sp0.x, sp0.y)
        tail.foreach { p =>
          val sp = p.to[ScreenPos]
          ctx.lineTo(sp.x, sp.y)
        }
    }
    ctx.stroke()

    ctx.beginPath()
    val roboSPos = roboMap.roboPos.pos.to[ScreenPos]
    ctx.arc(roboSPos.x, roboSPos.y, 3.5 * scale, 0, 2 * PI)
    ctx.strokeStyle = "black"
    ctx.fillStyle = "white"
    ctx.lineWidth = 1
    ctx.fill()
    ctx.stroke()
  }

}
