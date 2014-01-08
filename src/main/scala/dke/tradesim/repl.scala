package dke.tradesim

import scala.tools.nsc.interpreter._
import scala.tools.nsc.Settings
import dke.tradesim.db.SlickAdapter

// this code taken from http://stackoverflow.com/questions/18628516/embedded-scala-repl-interpreter-example-for-2-10
// Run this REPL from within sbt like this: sbt "runMain dke.tradesim.Repl"

object Repl extends App {
  val settings = new Settings
  settings.Yreplsync.value = true
  settings.deprecation.value = true

  //use when launching normally outside SBT
  //    settings.usejavacp.value = true

  //an alternative to 'usejavacp' setting, when launching from within SBT
  settings.embeddedDefaults[Repl.type]

  new TradesimRepl().process(settings)
}

class TradesimRepl extends ILoop {

  addThunk {
    intp.beQuietDuring {
      intp.addImports("scala.slick.driver.PostgresDriver.simple._",
                      "scala.slick.lifted.Query",
                      "scala.slick.session.Database",
                      "scala.slick.jdbc.{GetResult, StaticQuery => Q}",
                      "dke.tradesim.datetimeUtils._",
                      "dke.tradesim.db.{Adapter, SlickAdapter}",
                      "dke.tradesim.db.SlickAdapter._",
                      "Database.dynamicSession")
    }
  }

  override def loop(): Unit = {
//    intp.bind("e", "Double", 2.71828)
    SlickAdapter.withAdapter("jdbc:postgresql:tradesim", "org.postgresql.Driver") {
      super.loop()
    }
  }
}