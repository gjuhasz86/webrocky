package roborock.server

import java.time.LocalDateTime

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.typesafe.scalalogging.LazyLogging
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import roborock.core.MiioClient
import roborock.core.XiaomiClient

import io.circe.syntax._

case class Foo(bar: String)
class Routes(staticPath: String, miio: MiioClient, xc: XiaomiClient) extends LazyLogging {
  import io.circe.generic.auto._
  import Syntax._

  val route: Route = apiRoute ~ staticRoute

  lazy val apiRoute: Route = {
    p("api") {
      _.get("map") {
        println(LocalDateTime.now)
        val mapName = miio.reqMapName()
        val url = xc.mapUrl(mapName)
        val res = xc.getMapData(url).toVector
        complete(res.asJson)
      }
    }
  }

  lazy val staticRoute: Route = {
    pathEndOrSingleSlash {
      getFromFile(s"$staticPath/index.html")
    } ~
      getFromBrowseableDirectory(staticPath)

  }

}


