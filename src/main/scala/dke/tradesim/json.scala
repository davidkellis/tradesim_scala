package dke.tradesim

import org.json4s.JValue
import com.lambdaworks.jacks.JacksMapper

object json {
  def read[R : Manifest](json: String): R = {
    JacksMapper.readValue[R](json)
  }


  // JValueWithFilter
  // See https://twitter.com/softprops/status/332209774265499648
  // and https://gist.github.com/softprops/5542495
  implicit class LiftJValueWithFilter(self: JValue) extends JValueWithFilter(self, _ => true)

  class JValueWithFilter(self: JValue, p: JValue => Boolean) {
    def map[T](f: JValue => T): List[T] = self.filter(p).map(f)
    def flatMap[T](f: JValue => List[T]): List[T] = self.filter(p).flatMap(f)
    def foreach(f: JValue => Unit): Unit = self.filter(p).foreach(f)
    def withFilter(q: JValue => Boolean): JValueWithFilter = new JValueWithFilter(self, x => p(x) && q(x))
  }
}