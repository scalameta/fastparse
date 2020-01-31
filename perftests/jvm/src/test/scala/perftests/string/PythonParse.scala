package perftests.string

import perftests.Utils
import utest._
import scala.meta.internal.fastparse.all._
object PythonParse extends TestSuite {
  val crossValidationStream = getClass.getResourceAsStream("/cross_validation.py")
  val crossValidationSource = scala.io.Source.fromInputStream(crossValidationStream).mkString
  def crossValidationIterator(size: Int) = crossValidationSource.grouped(size)
  val parser = pythonparse.Statements.file_input

  val tests = Tests {
    'CrossValidation - {
      Utils.benchmarkAll(
        "PythonParse",
        parser,
        crossValidationSource, Some("def " + crossValidationSource),
        crossValidationIterator
      )
    }
  }
}