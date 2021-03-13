package roborock.core

import java.math.BigInteger

import io.circe.parser._
import roborock.core.MiioUtils._
import roborock.core.Utils._

import scala.annotation.tailrec

object Utils {
  def retry[A](f: => A)(timeoutFn: (Int, A) => Int): A = {
    @tailrec
    def loop(retry: Int): A = {
      val res = f
      val timeout = timeoutFn(retry, res)
      if (timeout < 0) {
        res
      } else {
        Thread.sleep(timeout)
        loop(retry + 1)
      }
    }
    loop(0)
  }
}

class MiioClient(tsp: TimestampProvider, ip: String, token: String) {


  def reqDeviceId(): Long = {
    val resp = MiioUtils.sendRaw(ip, 54321, msgToBytes(HelloMsg, 0xFFFFFFFF, token))
    val hexStr = resp.slice(8, 12).toHexString
    new BigInteger(hexStr, 16).longValue
  }

  def reqMapName(): String =
    retry {
      val res = send(MiioMsg.of("get_map_v1"))
      val Right(json) = parse(res)
      val mapName = json.asObject.get("result").get.asArray.get.head.asString.get
      mapName
    } {
      case (retry, "retry") if retry < 10 => 2000
      case (_, _) => -1
    }

  def reqHistoryMapName(mapId: String): String =
    retry {
      val res = send(MiioMsg.of("get_clean_record_map", s"[$mapId]"))
      val Right(json) = parse(res)
      val mapName = json.asObject.get("result").get.asArray.get.head.asString.get
      mapName
    } {
      case (retry, "retry") if retry < 10 => 2000
      case (_, _) => -1
    }

  def send(msg: MiioMsg): String = {
    println(s"Sending to device [$msg]")
    val devId = reqDeviceId()
    val resp = MiioUtils.sendRaw(ip, 54321, msgToBytes(msg, devId, token))
    val res = MiioUtils.decrypt(resp.drop(32), token)
    println(s"Received from device [$res]")
    res
  }

  def msgToBytes(msg: MiioMsg, devId: Long, token: String): Array[Byte] = msg match {
    case HelloMsg =>
      "21310020FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF".hexToByteArray
    case GenericMsg(payload) =>
      val magic = Array[Byte](0x21, 0x31)
      val unk = Array[Byte](0, 0, 0, 0)
      val deviceId = devId.formatted("%08X").hexToByteArray
      val timestamp = tsp.timestamp.formatted("%08X").hexToByteArray
      val initChecksum = token.hexToByteArray
      val encPayload = encrypt(payload, token)
      val len = (0x20 + encPayload.length).formatted("%04X").hexToByteArray

      val preCheckSum = magic ++ len ++ unk ++ deviceId ++ timestamp ++ initChecksum ++ encPayload
      val checksum = md5(preCheckSum)

      magic ++ len ++ unk ++ deviceId ++ timestamp ++ checksum ++ encPayload
  }

}
