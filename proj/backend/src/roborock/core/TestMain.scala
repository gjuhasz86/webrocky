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
    val xcp = new XiaomiClientProvider(user, passHash, country)
    val client = xcp.buildClient()
    val url = client.mapUrl("???")
    client.getMapData(url)
  }
}