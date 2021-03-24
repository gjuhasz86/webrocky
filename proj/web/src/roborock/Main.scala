package roborock
import java.lang.Math.PI

import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import org.scalajs.dom
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.ext.Ajax
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.html
import roborock.core.MiioMsg
import roborock.core.Pos
import roborock.mapparser.RoboMap
import roborock.mapparser.RoboMapBlock
import roborock.mapparser.Zone

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

case class ScreenPos(x: Int, y: Int)
sealed trait MapSelection
object MapSelection {
  case class ZoneSelection(zone: Zone) extends MapSelection
  case class TargetSelection(pos: Pos.World) extends MapSelection
  case object NoSelection extends MapSelection
}

object Main {
  import MapSelection._

  var allData: Array[Byte] = Array()
  var group: Int = 344
  var roboMap: RoboMap = _
  var scale: Double = 4.0
  var refreshInterval: Int = 5
  var autoRefresh: Boolean = true
  var autoRefreshHandle: Int = 0
  var rcSeqNum = 1
  var rcHandle = 0
  var mouseDownPos: Option[Pos.World] = None
  var selection: MapSelection = NoSelection
  var mousePos: ScreenPos = ScreenPos(0, 0)
  var rcUserVelocity = 0.2
  var rcUserOmega = 1.2
  var cleanRepeat = 1

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
  def main(canvas: html.Canvas, canvasOverlay: html.Canvas): Unit = {
    dom.document.getElementById("zoom-scale").textContent = scale.toString
    dom.document.getElementById("refresh-interval").textContent = refreshInterval.toString
    dom.document.getElementById("refresh-auto").setAttribute("checked", autoRefresh.toString)
    dom.document.getElementById("rc-velocity").textContent = f"$rcUserVelocity%.2f"
    dom.document.getElementById("rc-omega").textContent = f"$rcUserOmega%.1f"
    dom.document.getElementById("clean-repeat").textContent = cleanRepeat.toString
    dom.document.getElementById("action-zone-clean").setAttribute("disabled", "true")
    dom.document.getElementById("action-goto").setAttribute("disabled", "true")

    val renderCtx: CanvasRenderingContext2D =
      canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    renderCtx.canvas.width = dom.window.innerWidth.toInt
    renderCtx.canvas.height = dom.window.innerHeight.toInt

    val renderCtxOverlay: CanvasRenderingContext2D =
      canvasOverlay.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    renderCtxOverlay.canvas.width = dom.window.innerWidth.toInt
    renderCtxOverlay.canvas.height = dom.window.innerHeight.toInt

    def updateMap(): Unit = {
      println(s"Updating...")
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
      println(s"Updated...")
    }

    def sendCmd(msg: MiioMsg): Unit = {
      Ajax.post("api/rawcommand", msg.asJson.noSpaces, headers = Map("Content-Type" -> "application/json"))
    }

    def updateScreen(timeStamp: Double): Unit = {
      if (roboMap != null) {
        renderCtx.canvas.width = (roboMap.imageBlock.width * scale).toInt + 1
        renderCtx.canvas.height = (roboMap.imageBlock.height * scale).toInt + 1
        renderCtx.clearRect(0, 0, canvas.width, canvas.height)
        renderCtxOverlay.canvas.width = (roboMap.imageBlock.width * scale).toInt + 1
        renderCtxOverlay.canvas.height = (roboMap.imageBlock.height * scale).toInt + 1
        process(renderCtx)
        drawOverlay(renderCtxOverlay)
      }
      updateSelection()
    }

    def updateOverlay(): Unit = {
      renderCtxOverlay.clearRect(0, 0, renderCtxOverlay.canvas.width, renderCtxOverlay.canvas.height)
      drawOverlay(renderCtxOverlay)
    }

    def updateSelection(): Unit = {
      val text = selection match {
        case ZoneSelection(Zone(tl, br)) => s"[${tl.x},${tl.y},${br.x},${br.y}]"
        case TargetSelection(p) => s"[${p.x},${p.y}]"
        case MapSelection.NoSelection => "_"
      }
      dom.document.getElementById("coord-selection").textContent = text
    }

    dom.window.oncontextmenu = (e: dom.MouseEvent) => {e.preventDefault()}

    canvasOverlay.onmousemove = (e: dom.MouseEvent) => {
      val canvasRect = canvasOverlay.getBoundingClientRect()
      val sPos = ScreenPos(e.clientX.toInt - canvasRect.left.toInt, e.clientY.toInt - canvasRect.top.toInt)
      mousePos = sPos
      updateOverlay()
    }

    canvasOverlay.onmousedown = (e: dom.MouseEvent) => {
      val canvasRect = canvas.getBoundingClientRect()
      val sPos = ScreenPos(e.clientX.toInt - canvasRect.left.toInt, e.clientY.toInt - canvasRect.top.toInt)
      val wPos = sPos.to[Pos.World]

      println(s"$sPos ${sPos.to[Pos.Img]} ${sPos.to[Pos.Img].to[Pos.World]} ${sPos.to[Pos.World].to[Pos.Img]}")
      e.button match {
        case 0 if roboMap != null =>
          mouseDownPos = Some(wPos)
          selection = NoSelection
          println(wPos)
        case 2 if roboMap != null =>
          val msg = MiioMsg.of("app_goto_target", s"[${wPos.x},${wPos.y}]")
          println(msg)
          sendCmd(msg)
        case _ =>
      }
    }
    canvasOverlay.onmouseup = (e: dom.MouseEvent) => {
      val canvasRect = canvas.getBoundingClientRect()
      val sPos = ScreenPos(e.clientX.toInt - canvasRect.left.toInt, e.clientY.toInt - canvasRect.top.toInt)
      val wPos = sPos.to[Pos.World]

      (e.button, mouseDownPos) match {
        case (0, Some(pos)) if roboMap != null =>
          if (Math.abs(pos.x - wPos.x) < 150 && Math.abs(pos.y - wPos.y) < 150) {
            selection = TargetSelection(wPos)
            dom.document.getElementById("action-zone-clean").setAttribute("disabled", "true")
            dom.document.getElementById("action-goto").removeAttribute("disabled")
          } else {
            selection = ZoneSelection(Zone(
              topLeft = Pos.World(Math.min(wPos.x, pos.x), Math.min(wPos.y, pos.y)),
              bottomRight = Pos.World(Math.max(wPos.x, pos.x), Math.max(wPos.y, pos.y))
            ))
            dom.document.getElementById("action-zone-clean").removeAttribute("disabled")
            dom.document.getElementById("action-goto").setAttribute("disabled", "true")
          }
        case _ =>
      }
      mouseDownPos = None
      updateScreen(0.0)

    }

    var rcOmega: Option[Double] = None
    var rcVelocity: Option[Double] = None

    def sendRc(force: Boolean = false): Unit = {
      rcSeqNum = rcSeqNum + 1
      val omega = rcOmega.getOrElse(0.0)
      val velocity = rcVelocity.getOrElse(0.0)

      val msg =
        MiioMsg.of("app_rc_move", s"""[{"omega":$omega, "velocity":$velocity, "seqnum":$rcSeqNum, "duration":5000}]""")

      if (force || rcOmega.isDefined || rcVelocity.isDefined) {
        sendCmd(msg)
      }
    }

    dom.document.onkeyup = (e: dom.KeyboardEvent) => {

      e.keyCode match {
        case KeyCode.Up | KeyCode.Down =>
          rcVelocity = None
          sendRc(force = true)
        case KeyCode.Left | KeyCode.Right =>
          rcOmega = None
          sendRc(force = true)
        case _ =>
      }

      e.key match {
        case "+" => scale = scale + 1
        case "-" => scale = scale - 1
        //        case "r" => sendCmd(MiioMsg.of("app_rc_start"))
        //        case "s" => sendCmd(MiioMsg.of("app_rc_end"))
        case _ =>
      }

      dom.document.getElementById("zoom-scale").textContent = scale.toString
      dom.window.requestAnimationFrame(updateScreen)


    }

    dom.document.onkeydown = (e: dom.KeyboardEvent) => {
      (e.keyCode, rcOmega) match {
        case (KeyCode.Left, Some(om)) if om > 0 =>
        case (KeyCode.Left, _) =>
          rcOmega = Some(rcUserOmega)
          sendRc()
        case (KeyCode.Right, Some(om)) if om < 0 =>
        case (KeyCode.Right, _) =>
          rcOmega = Some(-rcUserOmega)
          sendRc()
        case _ =>
      }
      (e.keyCode, rcVelocity) match {
        case (KeyCode.Up, Some(v)) if v > 0 =>
        case (KeyCode.Up, _) =>
          rcVelocity = Some(rcUserVelocity)
          sendRc()
        case (KeyCode.Down, Some(v)) if v < 0 =>
        case (KeyCode.Down, _) =>
          rcVelocity = Some(-rcUserVelocity)
          sendRc()
        case _ =>
      }
    }
    rcHandle = dom.window.setInterval(() => {sendRc()}, 4000)


    dom.document.getElementById("zoom-plus").addEventListener("click", (e: dom.Event) => {
      scale = scale + 1
      dom.document.getElementById("zoom-scale").textContent = scale.toString
      dom.window.requestAnimationFrame(updateScreen)
    })
    dom.document.getElementById("zoom-minus").addEventListener("click", (e: dom.Event) => {
      if (scale > 1) {
        scale = scale - 1
        dom.document.getElementById("zoom-scale").textContent = scale.toString
        dom.window.requestAnimationFrame(updateScreen)
      }
    })
    dom.document.getElementById("refresh-auto").addEventListener("click", (e: dom.Event) => {
      autoRefresh = !autoRefresh
      if (autoRefresh) {
        autoRefreshHandle = dom.window.setInterval(() => updateMap(), refreshInterval * 1000)
      } else {
        dom.window.clearInterval(autoRefreshHandle)
      }
      dom.document.getElementById("refresh-auto").setAttribute("checked", autoRefresh.toString)
    })
    dom.document.getElementById("refresh-plus").addEventListener("click", (e: dom.Event) => {
      refreshInterval = refreshInterval + 1
      if (autoRefresh) {
        dom.window.clearInterval(autoRefreshHandle)
        autoRefreshHandle = dom.window.setInterval(() => updateMap(), refreshInterval * 1000)
      }
      dom.document.getElementById("refresh-interval").textContent = refreshInterval.toString

    })
    dom.document.getElementById("refresh-minus").addEventListener("click", (e: dom.Event) => {
      if (refreshInterval > 0) {
        refreshInterval = refreshInterval - 1
        if (autoRefresh) {
          dom.window.clearInterval(autoRefreshHandle)
          autoRefreshHandle = dom.window.setInterval(() => updateMap(), refreshInterval * 1000)
        }
        dom.document.getElementById("refresh-interval").textContent = refreshInterval.toString
      }
    })
    dom.document.getElementById("refresh-now").addEventListener("click", (e: dom.Event) => {
      updateMap()
    })

    dom.document.getElementById("action-stop").addEventListener("click", (e: dom.Event) => {
      sendCmd(MiioMsg.of("app_stop"))
    })
    dom.document.getElementById("action-dock").addEventListener("click", (e: dom.Event) => {
      sendCmd(MiioMsg.of("app_charge"))
    })
    dom.document.getElementById("action-spot").addEventListener("click", (e: dom.Event) => {
      sendCmd(MiioMsg.of("app_spot"))
    })
    dom.document.getElementById("action-fanspeed-1").addEventListener("click", (e: dom.Event) => {
      sendCmd(MiioMsg.of("set_custom_mode", "[101]"))
    })
    dom.document.getElementById("action-fanspeed-2").addEventListener("click", (e: dom.Event) => {
      sendCmd(MiioMsg.of("set_custom_mode", "[102]"))
    })
    dom.document.getElementById("action-fanspeed-3").addEventListener("click", (e: dom.Event) => {
      sendCmd(MiioMsg.of("set_custom_mode", "[103]"))
    })
    dom.document.getElementById("action-fanspeed-4").addEventListener("click", (e: dom.Event) => {
      sendCmd(MiioMsg.of("set_custom_mode", "[104]"))
    })
    dom.document.getElementById("action-rc-start").addEventListener("click", (e: dom.Event) => {
      sendCmd(MiioMsg.of("app_rc_start"))
    })
    dom.document.getElementById("action-rc-stop").addEventListener("click", (e: dom.Event) => {
      sendCmd(MiioMsg.of("app_rc_end"))
    })
    dom.document.getElementById("action-goto").addEventListener("click", (e: dom.Event) => {
      selection match {
        case TargetSelection(wPos) =>
          val msg = MiioMsg.of("app_goto_target", s"[${wPos.x},${wPos.y}]")
          sendCmd(msg)
        case _ =>
      }
    })
    dom.document.getElementById("action-zone-clean").addEventListener("click", (e: dom.Event) => {
      selection match {
        case ZoneSelection(zone) =>
          val tl = zone.topLeft
          val br = zone.bottomRight
          sendCmd(MiioMsg.of("app_zoned_clean", s"[${tl.x},${tl.y},${br.x},${br.y},$cleanRepeat]"))
        case _ =>
      }
    })

    dom.document.getElementById("action-rc-v-up").addEventListener("click", (e: dom.Event) => {
      if (rcUserVelocity < 0.3) {
        rcUserVelocity = Math.round((rcUserVelocity + 0.05) * 100) / 100.0
        dom.document.getElementById("rc-velocity").textContent = f"$rcUserVelocity%.2f"
      }
    })
    dom.document.getElementById("action-rc-v-down").addEventListener("click", (e: dom.Event) => {
      if (rcUserVelocity > 0) {
        rcUserVelocity = Math.round((rcUserVelocity - 0.05) * 100) / 100.0
        dom.document.getElementById("rc-velocity").textContent = f"$rcUserVelocity%.2f"
      }
    })
    dom.document.getElementById("action-rc-o-up").addEventListener("click", (e: dom.Event) => {
      if (rcUserOmega < 3.1) {
        rcUserOmega = Math.round((rcUserOmega + 0.2) * 10) / 10.0
        dom.document.getElementById("rc-omega").textContent = f"$rcUserOmega%.1f"
      }
    })
    dom.document.getElementById("action-rc-o-down").addEventListener("click", (e: dom.Event) => {
      if (rcUserOmega > 0) {
        rcUserOmega = Math.round((rcUserOmega - 0.2) * 10) / 10.0
        dom.document.getElementById("rc-omega").textContent = f"$rcUserOmega%.1f"
      }
    })
    dom.document.getElementById("action-repeat-up").addEventListener("click", (e: dom.Event) => {
      cleanRepeat = cleanRepeat + 1
      dom.document.getElementById("clean-repeat").textContent = cleanRepeat.toString
    })
    dom.document.getElementById("action-repeat-down").addEventListener("click", (e: dom.Event) => {
      if (cleanRepeat > 1) {
        cleanRepeat = cleanRepeat - 1
        dom.document.getElementById("clean-repeat").textContent = cleanRepeat.toString
      }
    })

    updateMap()
    autoRefreshHandle = dom.window.setInterval(() => updateMap(), refreshInterval * 1000)
  }

  def drawOverlay(ctx: CanvasRenderingContext2D): Unit = {
    ctx.strokeStyle = "black"
    ctx.lineWidth = 1
    ctx.beginPath()
    ctx.moveTo(mousePos.x, 0)
    ctx.lineTo(mousePos.x, ctx.canvas.height)
    ctx.moveTo(0, mousePos.y)
    ctx.lineTo(ctx.canvas.width, mousePos.y)
    ctx.stroke()

    mouseDownPos.foreach { mpos =>
      ctx.beginPath()
      ctx.lineWidth = 0.5 * scale
      ctx.fillStyle = "rgba(242, 242, 12, 0.6)"
      ctx.strokeStyle = "rgba(145, 145, 7, 0.6)"
      val spTl = mpos.to[ScreenPos]
      val spBr = mousePos
      ctx.rect(spTl.x, spTl.y, Math.abs(spBr.x - spTl.x), Math.abs(spBr.y - spTl.y))
      ctx.fill()
      ctx.stroke()
    }

    selection match {
      case ZoneSelection(zone) =>
        ctx.beginPath()
        ctx.lineWidth = 0.5 * scale
        ctx.fillStyle = "rgba(242, 242, 12, 0.6)"
        ctx.strokeStyle = "rgba(145, 145, 7, 0.6)"
        val spTl = zone.topLeft.to[ScreenPos]
        val spBr = zone.bottomRight.to[ScreenPos]
        ctx.rect(spTl.x, spTl.y, Math.abs(spBr.x - spTl.x), Math.abs(spBr.y - spTl.y))
        ctx.fill()
        ctx.stroke()
      case TargetSelection(wPos) =>
        val sPos = wPos.to[ScreenPos]

        ctx.beginPath()
        ctx.lineWidth = 0.5 * scale
        ctx.fillStyle = "rgba(255, 255, 0, 0.3)"
        val spTl = Pos.World(wPos.x - 750, wPos.y - 750).to[ScreenPos]
        val spBr = Pos.World(wPos.x + 750, wPos.y + 750).to[ScreenPos]
        ctx.rect(spTl.x, spTl.y, Math.abs(spBr.x - spTl.x), Math.abs(spBr.y - spTl.y))
        ctx.fill()

        ctx.beginPath()
        ctx.fillStyle = "rgba(242, 242, 12, 0.6)"
        ctx.arc(sPos.x, sPos.y, 3.5 * scale, 0, 2 * PI)
        ctx.fill()

      case MapSelection.NoSelection =>
    }

  }

  def process(ctx: CanvasRenderingContext2D): Unit = {
    val colors = Array("#e6194b", "#3cb44b", "#ffe119", "#4363d8", "#f58231", "#911eb4", "#46f0f0", "#f032e6", "#bcf60c", "#fabebe", "#008080", "#e6beff", "#9a6324", "#fffac8", "#800000", "#aaffc3", "#808000", "#ffd8b1", "#000075", "#808080", "#ffffff", "#000000")
    val colorMap = roboMap.imageBlock.image.distinct.filter(x => (x & 3) != 1).sorted.zip(colors).toMap
    ctx.beginPath()
    ctx.lineWidth = 1
    for (x <- 0 until roboMap.imageBlock.width; y <- 0 until roboMap.imageBlock.height) {
      val sPos = Pos.Img(x, y).to[ScreenPos]
      val draw = roboMap.pointAt(x, y) match {
        case 0 =>
          ctx.fillStyle = "white"
          false
        case c if (c & 3) == 0 =>
          ctx.fillStyle = colorMap(c)
          true
        case c if (c & 3) == 1 =>
          ctx.fillStyle = "black"
          true
        case c if (c & 3) == 3 =>
          ctx.fillStyle = colorMap(c)
          true
        case c =>
          println(c)
          false
      }
      if (draw) {
        ctx.fillRect(sPos.x - (scale / 2), sPos.y - (scale / 2), scale, scale)
      }
    }

    ctx.beginPath()
    ctx.lineWidth = 3.4 * scale
    ctx.strokeStyle = "rgba(255, 255, 255, 0.8)"
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
    ctx.lineWidth = (0.5 * scale).toInt + 1
    ctx.setLineDash(js.Array(3 * scale, 3 * scale))
    ctx.strokeStyle = "blue"
    roboMap.predPath.map(_.points).getOrElse(Nil) match {
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
    ctx.setLineDash(js.Array())

    ctx.beginPath()
    ctx.lineWidth = 0.5 * scale
    ctx.fillStyle = "rgba(10, 255, 30, 0.5)"
    ctx.strokeStyle = "rgba(0, 112, 9, 0.5)"
    roboMap.cleanedZones.map(_.zones).getOrElse(Nil).foreach { zone =>
      val spTl = zone.topLeft.to[ScreenPos]
      val spBr = zone.bottomRight.to[ScreenPos]
      ctx.rect(spTl.x, spTl.y, Math.abs(spBr.x - spTl.x), Math.abs(spBr.y - spTl.y))
      ctx.fill()
      ctx.stroke()
    }

    ctx.beginPath()
    ctx.lineWidth = 0.5 * scale
    ctx.fillStyle = "rgba(255, 5, 5, 0.5)"
    ctx.strokeStyle = "rgba(107, 0, 0, 0.5)"
    roboMap.noGoZones.map(_.zones).getOrElse(Nil).foreach { zone =>
      val spTl = zone.topLeft.to[ScreenPos]
      val spBr = zone.bottomRight.to[ScreenPos]
      ctx.rect(spTl.x, spTl.y, Math.abs(spBr.x - spTl.x), Math.abs(spBr.y - spTl.y))
      ctx.fill()
      ctx.stroke()
    }

    ctx.beginPath()
    ctx.lineWidth = 0.5 * scale
    ctx.fillStyle = "rgba(152, 10, 255, 0.5)"
    ctx.strokeStyle = "rgba(65, 0, 112, 0.5)"
    roboMap.noMopZones.map(_.zones).getOrElse(Nil).foreach { zone =>
      val spTl = zone.topLeft.to[ScreenPos]
      val spBr = zone.bottomRight.to[ScreenPos]
      ctx.rect(spTl.x, spTl.y, Math.abs(spBr.x - spTl.x), Math.abs(spBr.y - spTl.y))
      ctx.fill()
      ctx.stroke()
    }

    roboMap.chargerPos.foreach { chargerPos =>
      ctx.beginPath()
      val chargerSp = chargerPos.pos.to[ScreenPos]
      ctx.arc(chargerSp.x, chargerSp.y, 2 * scale, 0, 2 * PI)
      ctx.strokeStyle = "black"
      ctx.fillStyle = "green"
      ctx.lineWidth = 1
      ctx.fill()
      ctx.stroke()
    }

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
