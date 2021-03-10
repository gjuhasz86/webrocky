import mill._, scalalib._, mill.scalajslib._

trait Commmon extends ScalaModule {
  def scalaVersion = "2.13.4"
}

object proj extends Module {
  object shared extends ScalaJSModule with Commmon {
    override def scalaJSVersion = "1.3.0"
  }

  object backend extends ScalaModule with Commmon {
    override def moduleDeps = Seq(shared)
    override def scalacOptions = Seq("-deprecation", "-Ymacro-annotations", "-language:higherKinds")
    override def ivyDeps = Agg(
      ivy"com.typesafe.akka::akka-http:10.2.1",
      ivy"com.typesafe.akka::akka-stream:2.6.10",
      ivy"ch.qos.logback:logback-classic:1.2.3",
      ivy"com.typesafe.scala-logging::scala-logging:3.9.2",
      ivy"io.circe::circe-core:0.13.0",
      ivy"io.circe::circe-generic:0.13.0",
      ivy"io.circe::circe-parser:0.13.0",
      ivy"io.circe::circe-optics:0.13.0",
      ivy"com.github.pathikrit::better-files:3.9.1",
      ivy"de.heikoseeberger::akka-http-circe:1.35.0",
      ivy"com.typesafe:config:1.4.1",
      ivy"org.scalaj::scalaj-http:2.4.2"
    )
  }

  object web extends ScalaJSModule with Commmon {
    override def moduleDeps = Seq(shared)

    override def scalaJSVersion = "1.3.0"

    override def scalacOptions = List(
      "-language:higherKinds",
      "-Ymacro-annotations",
      "-deprecation",
    )

    override def ivyDeps = Agg(
      ivy"org.scala-lang.modules:scala-collection-contrib_sjs1_2.13:0.2.2",
      ivy"org.scala-js:scalajs-dom_sjs1_2.13:1.1.0",
      ivy"com.lihaoyi:scalatags_sjs1_2.13:0.9.2",
      ivy"me.shadaj:slinky-web_sjs1_2.13:0.6.6",
      ivy"io.circe:circe-core_sjs1_2.13:0.13.0",
      ivy"io.circe:circe-generic_sjs1_2.13:0.13.0",
      ivy"io.circe:circe-parser_sjs1_2.13:0.13.0",
      ivy"io.circe:circe-optics_sjs1_2.13:0.13.0",
      ivy"io.circe:circe-generic-extras_sjs1_2.13:0.13.0",
      ivy"com.raquo:laminar_sjs1_2.13:0.11.0",
      ivy"com.lihaoyi:scalarx_sjs1_2.13:0.4.3",
    )

  }

  def publicDir = T.sources {os.pwd / 'proj / 'public}
  def publicFiles = T {publicDir().flatMap(p => os.walk(p.path)).map(PathRef(_))}

  def dist = T {
    if (!os.exists(os.pwd / 'dist)) {os.makeDir(os.pwd / 'dist)}
    if (!os.exists(os.pwd / 'dist / 'public)) {os.makeDir(os.pwd / 'dist / 'public)}
    val jsFile = proj.web.fastOpt()
    val jsFileDir = jsFile.path.toNIO.getParent
    val jsFileName = jsFile.path.last
    publicFiles
    println(s"Copying [${jsFile.path}]")
    println(s"Copying [${os.Path(jsFileDir.toString) / s"$jsFileName.map"}]")
    os.copy.over(jsFile.path, os.pwd / 'dist / 'public / jsFileName)
    os.copy.over(os.Path(jsFileDir.toString) / s"$jsFileName.map", os.pwd / 'dist / 'public / s"$jsFileName.map")

    publicFiles()
      .map(_.path.relativeTo(os.pwd / 'proj / 'public))
      .foreach { f =>
        println(s"Copying [${os.pwd / 'proj / 'public / f}]")
        os.copy.over(os.pwd / 'proj / 'public / f, os.pwd / 'dist / 'public / f)
      }

  }
}