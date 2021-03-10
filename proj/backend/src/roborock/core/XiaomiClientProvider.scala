package roborock.core
import java.io.ByteArrayInputStream
import java.net.HttpCookie
import java.security.MessageDigest
import java.util.Base64
import java.util.zip.GZIPInputStream

import better.files._
import com.typesafe.config.ConfigFactory
import io.circe.Json
import scalaj.http.{Base64 => _, _}
import io.circe.parser._
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

object TestMain {
  def main(args: Array[String]): Unit = {
    val config = ConfigFactory.parseFile(File("dist/secrets.hocon").toJava)
    val user = config.getString("miio.user")
    val passHash = config.getString("miio.passHash")
    val country = config.getString("miio.country")
    val xcp = new XiaomiClientProvider(user, passHash, country)
    val client = xcp.buildClient()
    val url = client.mapUrl("???")
    client.getMapData(url)
  }
}

class XiaomiClient(agent: String, country: String, userId: String, serviceToken: String, ssecurity: String) {

  def getMapData(url: String): Array[Byte] = {
    val resp = Http(url).asBytes
    val compressed = resp.body
    val inputStream = new GZIPInputStream(new ByteArrayInputStream(compressed))
    val uncompressed = inputStream.byteArray
    uncompressed
  }

  def mapUrl(mapName: String): String = {
    val json = apiCall(apiUrl, "/home/getmapfileurl", Map("data" -> s"""{"obj_name":"$mapName"}"""))
    val url = json.asObject.get("result").get.asObject.get("url").get.asString.get
    url
  }

  def apiUrl: String = {
    val prefix = if (country == "cn") "" else s"$country."
    s"https://${prefix}api.io.mi.com/app"
  }

  def apiCall(baseUrl: String, relUrl: String, params: Map[String, String]): Json = {
    val nonce = rndNonce()
    val signedNonce = signNonce(nonce)
    val signature = genSignature(relUrl, signedNonce, nonce, params)
    apiCall0(baseUrl, relUrl, params, nonce, signature)
  }

  def apiCall0(baseUrl: String, relUrl: String, params: Map[String, String], nonce: String, signature: String): Json = {

    val resp =
      Http(s"$baseUrl$relUrl")
        .method("POST")
        .headers(
          "Accept-Encoding" -> "gzip",
          "User-Agent" -> agent,
          "Content-Type" -> "application/x-www-form-urlencoded",
          "x-xiaomi-protocal-flag-cli" -> "PROTOCAL-HTTP2")
        .cookies(Seq(
          new HttpCookie("userId", userId),
          new HttpCookie("yetAnotherServiceToken", serviceToken),
          new HttpCookie("serviceToken", serviceToken),
          new HttpCookie("locale", "en_GB"),
          new HttpCookie("timezone", "GMT+02:00"),
          new HttpCookie("is_daylight", "1"),
          new HttpCookie("dst_offset", "3600000"),
          new HttpCookie("channel", "MI_APP_STORE")
        ))
        .params(
          "data" -> params("data"),
          "signature" -> signature,
          "_nonce" -> nonce)
        .asString

    val Right(json) = parse(resp.body)
    json
  }

  def rndNonce(): String = {
    val bytes = scala.util.Random.nextBytes(12)
    Base64.getEncoder.encodeToString(bytes)
  }

  def signNonce(nonce: String): String = {
    val ssecBytes = Base64.getDecoder.decode(ssecurity)
    val nonceBytes = Base64.getDecoder.decode(nonce)
    val bytes = MessageDigest.getInstance("SHA-256").digest(ssecBytes ++ nonceBytes)
    Base64.getEncoder.encodeToString(bytes)
  }

  def genSignature(relUrl: String, signedNonce: String, nonce: String, params: Map[String, String]): String = {
    val sParams = List(relUrl, signedNonce, nonce) ++ params.toList.map { case (k, v) => s"$k=$v" }
    val msgBytes = sParams.mkString("&").getBytes("UTF-8")
    val sNonceBytes = Base64.getDecoder.decode(signedNonce)
    val secret = new SecretKeySpec(sNonceBytes, "HmacSHA256")
    val mac = Mac.getInstance("HmacSHA256")
    mac.init(secret)
    val hashBytes = mac.doFinal(msgBytes)
    Base64.getEncoder.encodeToString(hashBytes)
  }

}

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