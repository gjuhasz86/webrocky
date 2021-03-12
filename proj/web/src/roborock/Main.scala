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
import roborock.mapparser.RoboMap
import roborock.mapparser.RoboMapBlock

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

case class ScreenPos(x: Int, y: Int)
case class WorldPos(x: Int, y: Int)
case class ImgPos(x: Int, y: Int)

object Main {

  var allData: Array[Byte] = Array()
  var group: Int = 344
  var roboMap: RoboMap = _
  var scale = 3

  def toImgPos(worldPos: WorldPos): ImgPos =
    ImgPos((worldPos.x / 50) - roboMap.imageBlock.offset.x, (worldPos.y / 50) - roboMap.imageBlock.offset.y)

  def toScreenPos(imgPos: ImgPos): ScreenPos =
    ScreenPos(imgPos.y * scale, imgPos.x * scale)

  def toWorldPos(imgPos: ImgPos): WorldPos =
    WorldPos((roboMap.imageBlock.offset.x + imgPos.x) * 50, (roboMap.imageBlock.offset.y + imgPos.y) * 50)

  def toImgPos(screenPos: ScreenPos): ImgPos =
    ImgPos(screenPos.y / scale, screenPos.x / scale)

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
        renderCtx.canvas.width = roboMap.imageBlock.width * scale
        renderCtx.canvas.height = roboMap.imageBlock.height * scale
        renderCtx.clearRect(0, 0, canvas.width, canvas.height)
        process(renderCtx)
      }

    dom.window.oncontextmenu = (e: dom.MouseEvent) => {e.preventDefault()}

    canvas.onmousedown = (e: dom.MouseEvent) => {
      val sPos = ScreenPos(e.pageX.toInt, e.pageY.toInt)
      val iPos = toImgPos(sPos)
      val wPos = toWorldPos(iPos)
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
      val iPos = ImgPos(x, y)
      val sPos = toScreenPos(iPos)
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
      ctx.fillRect(sPos.x, sPos.y, scale, scale)
    }

    ctx.beginPath()
    ctx.lineWidth = 3 * scale
    ctx.strokeStyle = "red"
    roboMap.vacPath.points match {
      case Nil =>
      case head :: tail =>
        val sp0 = toScreenPos(toImgPos(WorldPos(head.x, head.y)))
        ctx.moveTo(sp0.x, sp0.y)
        tail.foreach { p =>
          val sp = toScreenPos(toImgPos(WorldPos(p.x, p.y)))
          ctx.lineTo(sp.x, sp.y)
        }
    }
    ctx.stroke()

    ctx.beginPath()
    val roboWPos = WorldPos(roboMap.roboPos.pos.x, roboMap.roboPos.pos.y)
    val roboIPos = toImgPos(roboWPos)
    val roboSPos = toScreenPos(roboIPos)
    ctx.arc(roboSPos.x, roboSPos.y, 4 * scale, 0, 2 * PI)
    ctx.strokeStyle = "black"
    ctx.fillStyle = "white"
    ctx.lineWidth = 1
    ctx.fill()
    ctx.stroke()
  }

}
