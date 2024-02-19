package ru.sanskrit.frontend

import cats.syntax.traverse.*

object Main {
  private def readFile(fileName: String): String = {
    val source = scala.io.Source.fromFile(fileName)
    val res    = source.getLines.mkString("\n")
    source.close()
    res
  }

  def main(args: Array[String]): Unit = {
    val files = args.map(readFile).toList
    for {
      parsed      <- files.traverse(file => parser.funcParser.rep0.parse(file).toOption.map(_._2))
      typechecked <- parsed.traverse(file => file.traverse(func => typecheck.inferFuncType(func)))
      result      <- typechecked.traverse(file => desugar.desugarProgram(file))
    } yield println(result)
  }
}