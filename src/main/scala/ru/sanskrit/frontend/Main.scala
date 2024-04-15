package ru.sanskrit.frontend

import cats.syntax.traverse.*
import ru.sanskrit.backend.interpreter

object Main {
  private def readFile(fileName: String): String = {
    val source = scala.io.Source.fromFile(fileName)
    val res    = source.getLines.mkString("\n")
    source.close()
    res
  }

  def main(args: Array[String]): Unit = {
    val files = args.map(readFile).toList
    (for {
      parsed      <- files.traverse(file => parser.funcParser.rep0.parse(file).toOption.map(_._2)).toRight("Parsing error")
      typechecked <- parsed.traverse(file => file.traverse(func => typecheck.inferFuncType(func)))
        .left.map(e => s"Typing error: ${e.cause} at [${e.position.begin.line}:${e.position.begin.col}, ${e.position.end.line}:${e.position.end.col}]")
      desugared <- typechecked.traverse(file => desugar.desugarProgram(file)).toRight("Desugaring error")
      interpreted <- desugared.traverse(expr => interpreter.run(expr).toRight("Interpreting error"))
    } yield interpreted).fold(println, res => println(res))
  }
}