package roborock.core

import better.files._
import com.typesafe.config.ConfigFactory
import scalaj.http.{Base64 => _}

object TestMain {
  def main(args: Array[String]): Unit = {
    val config = ConfigFactory.parseFile(File("dist/secrets.hocon").toJava)
    val user = config.getString("xiaomi.user")
    val passHash = config.getString("xiaomi.passHash")
    val country = config.getString("xiaomi.country")
    val ip = config.getString("miio.ip")
    val token = config.getString("miio.token")

    val miio = new MiioClient(DefaultTimestampProvider, ip, token)
    val mapName = miio.reqMapName()
    println(mapName)

    val xcp = new XiaomiClientProvider(user, passHash, country)
    val client: XiaomiClient = xcp.buildClient()


    val url = client.mapUrl(mapName)
    client.getMapData(url)
  }
}