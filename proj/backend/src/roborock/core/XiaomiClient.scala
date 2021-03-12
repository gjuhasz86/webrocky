package roborock.core

import java.io.ByteArrayInputStream
import java.net.HttpCookie
import java.security.MessageDigest
import java.util.Base64
import java.util.zip.GZIPInputStream

import better.files._
import io.circe.Json
import io.circe.parser._
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import scalaj.http.{Base64 => _}
import scalaj.http._

// implementation based on
// https://github.com/PiotrMachowski/Home-Assistant-custom-components-Xiaomi-Cloud-Map-Extractor/blob/bdaec4aa14c2f30b69d2c966bf1fbeab6e5aaff7/custom_components/xiaomi_cloud_map_extractor/xiaomi_cloud_connector.py
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

    println(s"Sending request to Xiaomi [$baseUrl$relUrl]")
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

    parse(resp.body) match {
      case Left(err) =>
        println(s"Could not parse mesage: [${resp.body}] $err")
        Json.Null
      case Right(json) =>
        println(s"Received response from Xiaomi: [${json.noSpaces}]")
        json
    }
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