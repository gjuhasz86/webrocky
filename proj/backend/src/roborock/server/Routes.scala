package roborock.server

import java.time.LocalDateTime

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.typesafe.scalalogging.LazyLogging
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import roborock.core.MapProvider

case class Foo(bar: String)
class Routes(staticPath: String, mapProvider: MapProvider) extends LazyLogging {
  import io.circe.generic.auto._
  import Syntax._

  val route = apiRoute ~ staticRoute

  lazy val apiRoute: Route = {
    p("api") {
      _.get("debug") {
        println(LocalDateTime.now)
        complete("ok")
      }
    }
  }

  lazy val staticRoute = {
    pathEndOrSingleSlash {
      getFromFile(s"$staticPath/index.html")
    } ~
      getFromBrowseableDirectory(staticPath)

  }

}


