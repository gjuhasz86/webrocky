package roborock.server

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Directives.{get => get0}
import akka.http.scaladsl.server.Directives.{post => post0}
import akka.http.scaladsl.server.Route

object Syntax {

  def post(str: String)(f: => Route): Route =
    pathPrefix(str) { pathEnd { post0 { f } } }
  def get(str: String)(f: => Route): Route =
    pathPrefix(str) { pathEnd { get0 { f } } }

  def p(str: String)(f: Syntax.type => Route): Route =
    pathPrefix(str) { f(Syntax) }

  implicit class RichRoute(self: Route) {
    def get(str: String)(f: => Route): Route = self ~ Syntax.get(str)(f)
    def post(str: String)(f: => Route): Route = self ~ Syntax.post(str)(f)
    def p(str: String)(f: Syntax.type => Route): Route = Syntax.p(str)(f)
  }
}
