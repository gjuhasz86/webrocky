package roborock.core

import java.security.MessageDigest

import io.circe.parser._
import scalaj.http._
import scalaj.http.{Base64 => _}

// implementation based on
// https://github.com/PiotrMachowski/Home-Assistant-custom-components-Xiaomi-Cloud-Map-Extractor/blob/bdaec4aa14c2f30b69d2c966bf1fbeab6e5aaff7/custom_components/xiaomi_cloud_map_extractor/xiaomi_cloud_connector.py
class XiaomiClientProvider(userName: String, passHash: String, country: String) {
  import XiaomiClientProvider._

  val agent: String = rndAgent()
  val deviceId: String = rndDeviceId()

  def buildClient(): XiaomiClient = {
    val sign = loginStep1()
    val (userId, ssecurity, location) = loginStep2(sign)
    val serviceToken = loginStep3(location)
    new XiaomiClient(agent, country, userId.toString, serviceToken, ssecurity)
  }

  def loginStep1(): String = {
    println(agent)
    println(deviceId)
    val resp =
      Http("https://account.xiaomi.com/pass/serviceLogin?sid=xiaomiio&_json=true")
        .headers("User-Agent" -> agent, "Content-Type" -> "application/x-www-form-urlencoded")
        .cookie("userId", userName)
        .cookie("sdkVersion", "accountsdk-18.8.15")
        .cookie("deviceId", deviceId)
        .asString

    resp.cookies foreach println

    val rawJson = resp.body.drop(11) // drop '&&&START&&&' prefix
    val Right(json) = parse(rawJson)
    println(json.spaces2)

    val sign = json.asObject.get("_sign").get.asString.get
    sign
  }

  def loginStep2(sign: String): (Long, String, String) = {

    val resp =
      Http("https://account.xiaomi.com/pass/serviceLoginAuth2")
        .headers("User-Agent" -> agent, "Content-Type" -> "application/x-www-form-urlencoded")
        .cookie("userId", userName)
        .cookie("sdkVersion", "accountsdk-18.8.15")
        .cookie("deviceId", deviceId)
        .params(
          "sid" -> "xiaomiio",
          "hash" -> passHash,
          "callback" -> "https://sts.api.io.mi.com/sts",
          "qs" -> "%3Fsid%3Dxiaomiio%26_json%3Dtrue",
          "user" -> userName,
          "_sign" -> sign,
          "_json" -> "true")
        .postForm
        .asString

    resp.cookies foreach println
    val rawJson = resp.body.drop(11) // drop '&&&START&&&' prefix
    val Right(json) = parse(rawJson)
    println(json.spaces2)

    val ssecurity = json.asObject.get("ssecurity").get.asString.get
    val userId = json.asObject.get("userId").get.asNumber.get.toLong.get
    //    val cUserId = json.asObject.get("cUserId").get.asString.get
    //    val passToken = json.asObject.get("passToken").get.asString.get
    val location = json.asObject.get("location").get.asString.get
    //    val code = json.asObject.get("code").get.asNumber.get.toInt
    (userId, ssecurity, location)
  }

  def loginStep3(location: String): String = {
    val resp =
      Http(location)
        .headers("User-Agent" -> agent, "Content-Type" -> "application/x-www-form-urlencoded")
        .cookie("userId", userName)
        .asString

    resp.cookies foreach println
    println(resp.body)
    //    val rawJson = resp.body.drop(11) // drop '&&&START&&&' prefix
    //    val Right(json) = parse(rawJson)
    //    println(json.spaces2)
    resp.cookies.find(_.getName == "serviceToken").get.getValue
  }
}

object XiaomiClientProvider {

  val rnd = new scala.util.Random

  def rndDeviceId(): String =
    Seq.fill(6)(rnd.between(97, 123)).map(_.toChar).mkString

  def rndAgent(): String = {
    val id = Seq.fill(13)(rnd.between(65, 70)).map(_.toChar).mkString
    s"Android-7.1.1-1.0.0-ONEPLUS A3010-136-$id APP/xiaomi.smarthome APPV/62830"
  }

  def md5(s: String): Array[Byte] =
    MessageDigest.getInstance("MD5").digest(s.getBytes("UTF-8"))

}