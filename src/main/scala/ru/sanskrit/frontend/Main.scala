package ru.sanskrit.frontend

import cats.syntax.traverse.*
import ru.sanskrit.backend.interpreter
import ru.sanskrit.frontend.typecheck.TypeCheckError

object Main {
  private def readFile(fileName: String): String = {
    val source = scala.io.Source.fromFile(fileName)
    val res    = source.getLines.mkString("\n")
    source.close()
    res
  }

  private def createTypeCheckError(e: TypeCheckError): String =
    s"Typing error: ${e.cause} at [${e.position.begin.line}:${e.position.begin.col}, ${e.position.end.line}:${e.position.end.col}]"

  def main(args: Array[String]): Unit = {
    val files = args.map(readFile).toList
    (for {
      parsed      <- files.traverse(file => parser.parseFile(file)).toRight("Parsing error")
      typechecked <- parsed.traverse(typecheck.typecheckAndLink).left.map(createTypeCheckError)
      desugared <- typechecked.traverse(file => desugar.desugarProgram(file)).toRight("Desugaring error")
      interpreted <- desugared.traverse(expr => interpreter.run(expr).toRight("Interpreting error"))
    } yield interpreted).fold(println, res => println(res))
  }
}
