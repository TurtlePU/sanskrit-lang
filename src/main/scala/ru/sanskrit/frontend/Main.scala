package ru.sanskrit.frontend

import cats.syntax.traverse.*
import ru.sanskrit.backend.interpreter
import ru.sanskrit.frontend.typecheck.TypeCheckError
import ru.sanskrit.backend.translator
import ru.sanskrit.common.*

object Main {
  enum Option:
    case Debug

  class Options(options: Set[Option]) {
    def debugEnabled: Boolean = options.contains(Option.Debug)
  }

  private def parseOptions(args: Array[String]): (Array[String], Options) = {
    val options = args.filter(_.startsWith("-")).collect {
      case "-D" | "-d" | "--debug" => Option.Debug
    }.toSet
    (args.filterNot(_.startsWith("-")), Options(options))
  }

  private def readFile(fileName: String): String = {
    val source = scala.io.Source.fromFile(fileName)
    val res    = source.getLines.mkString("\n")
    source.close()
    res
  }

  private def createTypeCheckError(e: TypeCheckError): String =
    s"Typing error: ${e.cause} at [${e.position.begin.line}:${e.position.begin.col}, ${e.position.end.line}:${e.position.end.col}]"

  private def runBackend(expr: Expr, mode: String): Either[String, String | Val] =
    if mode == "translate" then
      translator.run(expr).toRight("Translation error")
    else if mode == "interpret" then
      interpreter.run(expr).toRight("Interpreting error")
    else Left("Unknown run mode")

  def main(args: Array[String]): Unit = {
    val runMode = args(0)
    val (args1, options) = parseOptions(args.tail)
    val files = args1.map(readFile).toList

    (for {
      parsed      <- files.traverse(file => parser.parseFile(file)).toRight("Parsing error")
      _ = if (options.debugEnabled) then println(parsed) else ()
      typechecked <- parsed.traverse(typecheck.typecheckAndLink).left.map(createTypeCheckError)
      _ = if (options.debugEnabled) then println(typechecked) else ()
      desugared   <- typechecked.traverse(file => desugar.desugarProgram(file)).toRight("Desugaring error")
      _ = if (options.debugEnabled) then println(desugared) else ()
      result      <- desugared.traverse(expr => runBackend(expr, runMode))
    } yield result).fold(println, res => println(res))
  }
}
