package roborock
import java.lang.Math.PI

import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.Event
import org.scalajs.dom.XMLHttpRequest
import org.scalajs.dom.html
import io.circe.parser._
import roborock.mapparser.Pos
import roborock.mapparser.RoboMap
import roborock.mapparser.RoboMapBlock

import scala.scalajs.js

object Main {

  var allData: Array[Byte] = Array()
  var group: Int = 344
  var roboMap: RoboMap = _
  val scale = 3

  @JSExportTopLevel("main")
  def main(canvas: html.Canvas): Unit = {
    val renderCtx: CanvasRenderingContext2D =
      canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    //    renderCtx.canvas.width = 3000 //dom.window.innerWidth.toInt
    //    renderCtx.canvas.height = 3000 //dom.window.innerHeight.toInt
    renderCtx.canvas.width = dom.window.innerWidth.toInt
    renderCtx.canvas.height = dom.window.innerHeight.toInt

    def updateMap(): Unit = {
      val xhr = new XMLHttpRequest()
      xhr.open("GET", "api/map")
      xhr.onload = { e: Event =>
        if (xhr.status == 200) {
          val Right(data) = decode[Vector[Byte]](xhr.responseText)
          allData = data.toArray
          roboMap = RoboMap(RoboMapBlock.parseMany(data.drop(20)))
          renderCtx.canvas.width = roboMap.imageBlock.width * scale
          renderCtx.canvas.height = roboMap.imageBlock.height * scale
          println(s"[${js.Date.now()}] Map updated $roboMap")
          dom.window.requestAnimationFrame(updateScreen)
        } else {
          println("err")
        }
      }
      xhr.send()
    }

    def updateScreen(timeStamp: Double): Unit = {
      renderCtx.clearRect(0, 0, canvas.width, canvas.height)
      process(renderCtx)
    }

    canvas.onmousedown = (e: dom.MouseEvent) => {
      println((e.pageY.toInt / scale + roboMap.imageBlock.offset.x) * 50, (e.pageX.toInt / scale + roboMap.imageBlock.offset.y) * 50)
    }

    dom.window.setInterval(() => updateMap(), 10000)
  }

  def process(ctx: CanvasRenderingContext2D): Unit = {
    val colors = Array("#e6194b", "#3cb44b", "#ffe119", "#4363d8", "#f58231", "#911eb4", "#46f0f0", "#f032e6", "#bcf60c", "#fabebe", "#008080", "#e6beff", "#9a6324", "#fffac8", "#800000", "#aaffc3", "#808000", "#ffd8b1", "#000075", "#808080", "#ffffff", "#000000")
    val colorMap = roboMap.imageBlock.image.distinct.filter(x => (x & 3) != 1).sorted.zip(colors).toMap
    for (x <- 0 until roboMap.imageBlock.width; y <- 0 until roboMap.imageBlock.height) {
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
      ctx.fillRect(y * scale, x * scale, scale, scale)
    }

    val roboPos = Pos(roboMap.roboPos.pos.x / 50 - roboMap.imageBlock.offset.x, roboMap.roboPos.pos.y / 50 - roboMap.imageBlock.offset.y)
    ctx.arc(roboPos.y * scale, roboPos.x * scale, 4 * scale, 0, 2 * PI)
    ctx.strokeStyle = "black"
    ctx.fillStyle = "white"
    ctx.fill()
    ctx.stroke()
  }

}
