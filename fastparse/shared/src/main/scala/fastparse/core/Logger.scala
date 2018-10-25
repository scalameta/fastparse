package scala.meta.internal.fastparse.core
import acyclic.file
case class Logger(f: String => Unit)
object Logger {
  implicit val stdout = Logger(println)
}
