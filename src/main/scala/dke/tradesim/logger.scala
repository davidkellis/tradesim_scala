package dke.tradesim

import scala.annotation.elidable

object logger {
  @elidable(elidable.FINER)    // FINER = 400
  def verbose(msg: String) = println(msg)

  @elidable(elidable.FINE)    // FINE = 500
  def debug(msg: String) = println(msg)

  @elidable(elidable.INFO)    // INFO = 800
  def info(msg: String) = println(msg)

  @elidable(elidable.WARNING)    // WARNING = 900
  def warning(msg: String) = println(msg)
}
