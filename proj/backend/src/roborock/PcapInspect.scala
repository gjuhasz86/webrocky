package roborock
import better.files._
import com.typesafe.config.ConfigFactory
import roborock.core.MiioUtils._


/*
Decrypts communication to and from a miio device captured by tshark.
Properly formatted message file is expected as a first parameter.

Example command:
sudo tshark -i 1 -f 'udp port 54321' -T fields -e ip.src -e udp.srcport -e data > capture.msg

*/
object PcapInspect {
  def main(args: Array[String]): Unit = {
    val config = ConfigFactory.parseFile(File("dist/secrets.hocon").toJava)
    val token = config.getString("miio.token")

    val f = args(0).toFile
    f.contentAsString.linesIterator.toList
      .map(_.split("\t"))
      .foreach { case Array(ip, port, msg) =>
        val decMsg = decrypt(msg.hexToByteArray.drop(32), token)
        println(s"$ip\t$port\t$decMsg")
      }

  }
}
