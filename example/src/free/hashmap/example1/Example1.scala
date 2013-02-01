package free
package hashmap
package example1

import java.util

import RichHashMap._

object Before {
  def main(args: Array[String]) {
    val conf = new util.HashMap[String, String]
    val q = conf put ("ak", "av")
    println("q: " + q)
    val r = conf get "ak"
    println("r: " + r)
    conf putPrintln ("ak", "av")
    println("li")
    conf putPrintln ("bk", "bv")
    val s = conf get "ak"
    val t = conf get "bk"
    println("r: " + r)
    println("s: " + s)
    println("t: " + t)
  }
}

object After {
  import KeyValueStoreEffectInterpreter.{println => kprintln, _}

  def main(args: Array[String]) {
    val conf = new util.HashMap[String, String]

    val w =
      for {
        q <- put("ak", "av")
        _ <- kprintln("q: " + q)
        r <- get("ak")
        _ <- kprintln("r: " + r)
        _ <- putPrintln("ak", "av")
        _ <- kprintln("li")
        _ <- putPrintln("bk", "bv")
        s <- get("ak")
        t <- get("bk")
        _ <- kprintln("r: " + r)
        _ <- kprintln("s: " + s)
        _ <- kprintln("t: " + t)
      } yield ()

    w runJHashMap conf
  }
}

object Example1 {
  def main(args: Array[String]) {
    println("==== BEFORE ====")
    Before.main(args)
    println
    println("==== AFTER ====")
    println
    After.main(args)
    println("===============")
  }
}