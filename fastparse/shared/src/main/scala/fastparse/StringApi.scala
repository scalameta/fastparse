package scala.meta.internal.fastparse
// import acyclic.file
import scala.meta.internal.fastparse.core.{ParserApi, ParserApiImpl}

import language.experimental.macros
import scala.meta.internal.fastparse.parsers.Intrinsics
import scala.meta.internal.fastparse.parsers.Terminals.AnyElems
import scala.meta.internal.fastparse.utils.ElemSetHelper.CharBitSetHelper
import scala.meta.internal.fastparse


class StringApi() extends Api[Char, String](
  implicitly, CharBitSetHelper, StringReprOps, CharBitSetHelper.ordering
) {

  val AnyChar = parsers.Terminals.AnyElem[Char, String]("AnyChar")
  def AnyChars(count: Int) = AnyElems[Char, String]("AnyChars", count)

  val AnyElem = AnyChar
  def AnyElem(count: Int) = AnyChars(count)

  object ElemPred extends ElemPred{
    def create(pred: Char => Boolean, precompute: Boolean) =
      Intrinsics.ElemPred("CharPred", pred, precompute)
  }
  object ElemsWhile extends ElemsWhile{
    def create(pred: Char => Boolean, min: Int = 1, precompute: Boolean) =
      Intrinsics.ElemsWhile("CharsWhile", pred, min, precompute)
  }

  def ElemIn(strings: collection.Seq[Char]*) = {
    Intrinsics.ElemIn[Char, String]("CharIn", strings.map(_.toIndexedSeq))
  }

  def ElemsWhileIn(strings: collection.Seq[Char], min: Int = 1) = {
    Intrinsics.ElemsWhileIn[Char, String]("CharsWhileIn", collection.Seq(strings), min)
  }

  def CharIn(strings: collection.Seq[Char]*) = ElemIn(strings:_*)
  def CharsWhileIn(strings: collection.Seq[Char], min: Int = 1) = ElemsWhileIn(strings, min)


  val CharsWhile = ElemsWhile
  val CharPred = ElemPred


  def StringIn(strings: String*) = SeqIn(strings: _*)
  def StringInIgnoreCase(strings: String*) =
    Intrinsics.StringInIgnoreCase[Char, String](strings: _*)

  val CharPredicates = fastparse.CharPredicates
  val IgnoreCase = parsers.Terminals.IgnoreCase

  implicit def LiteralStr(s: String): P0 =
    if (s.length == 1) parsers.Terminals.ElemLiteral(s(0))
    else parsers.Terminals.Literal(s)
}

object all extends StringApi{
  implicit def parserApi[T, V](p: T)(implicit c: T => core.Parser[V, Char, String]): ParserApi[V, Char, String] =
    new ParserApiImpl[V, Char, String](p)
}
object noApi extends StringApi
