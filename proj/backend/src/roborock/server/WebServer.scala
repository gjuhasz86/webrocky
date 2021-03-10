package roborock.server

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import better.files._
import com.typesafe.config.ConfigFactory
import roborock.core.XiaomiClientProvider

import scala.annotation.tailrec
import scala.concurrent.ExecutionContextExecutor
import scala.io.StdIn

// runs a webserver that serves static files from a
// directory given as a command-line argument
// and also serves api calls
object WebServer {
  def main(args: Array[String]): Unit = {
    implicit val system: ActorSystem = ActorSystem()
    implicit val executionContext: ExecutionContextExecutor = system.dispatcher

    val staticPath = args.head

    val config = ConfigFactory.parseFile(File("secrets.hocon").toJava)
    val user = config.getString("xiaomi.user")
    val passHash = config.getString("xiaomi.user")
    val country = config.getString("xiaomi.country")
    val mp = new XiaomiClientProvider(user, passHash, country)

    val bindingFuture = Http().newServerAt("0.0.0.0", 4201).bindFlow(new Routes(staticPath, mp).route)
    println(s"Server online at http://localhost:4201/\nType 'exit' without quotes and press RETURN to stop...")

    exitLoop()

    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }

  @tailrec
  def exitLoop(): Int = {
    val line = StdIn.readLine() // let it run until user presses return
    if (line == "exit") 0 else exitLoop()
  }
}
