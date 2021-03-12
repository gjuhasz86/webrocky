package roborock.core
import java.util.concurrent.atomic.AtomicInteger

import scala.util.Random

sealed trait MiioMsg
case object HelloMsg extends MiioMsg
case class GenericMsg(payload: String) extends MiioMsg

object MiioMsg {
  val id = new AtomicInteger(Random.between(1000, 2000))
  def nextId(): Int = id.getAndIncrement()
  def of(cmd: String, params: String = "[]"): MiioMsg =
    GenericMsg(s"""{"id": ${nextId()}, "method": "$cmd", "params": $params}""")
}