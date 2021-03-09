package roborock
import scala.scalajs.js.annotation.JSExportTopLevel

import org.scalajs.dom
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.Event
import org.scalajs.dom.XMLHttpRequest
import org.scalajs.dom.html

import scala.scalajs.js
import scala.scalajs.js.JSON


object Main {

  var allData: Array[Int] = Array()
  var group: Int = 344
  //  var group: Int = 200

  @JSExportTopLevel("main")
  def main(canvas: html.Canvas): Unit = {
    val renderCtx: CanvasRenderingContext2D =
      canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    //    renderCtx.canvas.width = 3000 //dom.window.innerWidth.toInt
    //    renderCtx.canvas.height = 3000 //dom.window.innerHeight.toInt
    renderCtx.canvas.width = dom.window.innerWidth.toInt
    renderCtx.canvas.height = dom.window.innerHeight.toInt

    val xhr = new XMLHttpRequest()
    xhr.open("GET", "mapdata.json")
    xhr.onload = { (e: Event) =>
      if (xhr.status == 200) {
        val data = JSON.parse(xhr.responseText).asInstanceOf[js.Array[Int]]
        allData = data.toArray
        dom.window.requestAnimationFrame(updateScreen)
      } else {
        println("err")
      }
    }
    xhr.send()

    def updateScreen(timeStamp: Double): Unit = {
      renderCtx.clearRect(0, 0, canvas.width, canvas.height)
      process(renderCtx)
    }

    canvas.onmousedown = (e: dom.MouseEvent) => {
      println(e.pageY * 10 + 287 * 50, e.pageX * 10 + 237 * 50)
    }

    canvas.addEventListener("wheel", (e: dom.WheelEvent) => {
      if (e.deltaY.sign > 0) {
        group = group + 1
      } else if (e.deltaY.sign < 0) {
        group = group - 1
      }
      dom.window.requestAnimationFrame(updateScreen)
    })
  }

  def process(ctx: CanvasRenderingContext2D) = {
    val mapData = allData.drop(20).drop(28).take(138288)
    val arrs = mapData.grouped(group).toArray
    val colors = Array("#e6194b", "#3cb44b", "#ffe119", "#4363d8", "#f58231", "#911eb4", "#46f0f0", "#f032e6", "#bcf60c", "#fabebe", "#008080", "#e6beff", "#9a6324", "#fffac8", "#800000", "#aaffc3", "#808000", "#ffd8b1", "#000075", "#808080", "#ffffff", "#000000")
    val colorMap = mapData.distinct.filter(x => (x & 3) != 1).sorted.zip(colors).toMap
    //    println(("00000000" + mapData.distinct.sorted.toVector(sel).toBinaryString).takeRight(8))


    for (x <- arrs.indices; y <- arrs(x).indices) {
      arrs(x)(y) match {
        case 0 =>
        case c if (c & 3) == 0 =>
          ctx.fillStyle = colorMap(c)
          ctx.fillRect(x * 2, y * 2, 2, 2)
        //          ctx.fillRect(x * 5, y * 5, 5, 5)
        case c if (c & 3) == 1 =>
          ctx.fillStyle = "black"
          ctx.fillRect(x * 2, y * 2, 2, 2)
        //          ctx.fillRect(x * 5, y * 5, 5, 5)
        case c if (c & 3) == 3 =>
          ctx.fillStyle = colorMap(c)
          ctx.fillRect(x * 2, y * 2, 2, 2)
        //          ctx.fillRect(x * 5, y * 5, 5, 5)
        case c =>
          println(c)
      }
    }
  }
}
