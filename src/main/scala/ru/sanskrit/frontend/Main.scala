package ru.sanskrit.frontend

import cats.syntax.traverse.*
import ru.sanskrit.backend.interpreter
import ru.sanskrit.frontend.typecheck.TypeCheckError
import ru.sanskrit.backend.translator
import ru.sanskrit.common.*
import cats.instances.string
import scala.Option


object Main {
  enum Option1:
    case Debug
  
  private var workingFiles: List[String] = List("")
  def setWorkingFiles(value: List[String]): Unit = {
    workingFiles = value
  }
  def getWorkingFiles: List[String] = workingFiles
  
  private var workingOptions: ru.sanskrit.frontend.Main.Options = parseOptions(Array(""))(1)
  def setWorkingOptions(value: ru.sanskrit.frontend.Main.Options): Unit = {
    workingOptions = value
  }
  def GetWorkingOptions: ru.sanskrit.frontend.Main.Options = workingOptions
  
  private var workingRunMode: String = ""
  def setWorkingRunMode(value: String): Unit = {
    workingRunMode = value
  }
  def getWorkingRunMode: String = workingRunMode
  
  private var parsedWorkingFile: List[List[ru.sanskrit.frontend.syntax.Func[Option]]] = List[List[ru.sanskrit.frontend.syntax.Func[Option]]]()
  def setParsedWorkingFile(value: List[List[ru.sanskrit.frontend.syntax.Func[Option]]]): Unit = {
    parsedWorkingFile = value
  }
  def getParsedWorkingFile: List[List[ru.sanskrit.frontend.syntax.Func[Option]]] = parsedWorkingFile
  
  private var typecheckedFile: List[List[ru.sanskrit.frontend.syntax.Func[cats.Id]]] = List[List[ru.sanskrit.frontend.syntax.Func[cats.Id]]]()
  def setTypecheckedFiles(value: List[List[ru.sanskrit.frontend.syntax.Func[cats.Id]]]): Unit = {
    typecheckedFile = value
  }
  def getTypecheckedFiles: List[List[ru.sanskrit.frontend.syntax.Func[cats.Id]]] = typecheckedFile
  
  private var desugaredFile: List[ru.sanskrit.common.Expr] = List[ru.sanskrit.common.Expr]()
  def setDesugaredFiles(value: List[ru.sanskrit.common.Expr]): Unit = {
    desugaredFile = value
  }
  def getDesugaredFiles: List[ru.sanskrit.common.Expr] = desugaredFile

  class Options(options: Set[Option1]) {
    def debugEnabled: Boolean = options.contains(Option1.Debug)
  }

  private def parseOptions(args: Array[String]): (Array[String], Options) = {
    val options = args.filter(_.startsWith("-")).collect {
      case "-D" | "-d" | "--debug" => Option1.Debug
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

  def saveFiles(args: Array[String]): Unit = {
    val runMode = args(0)
    val (args1, options) = parseOptions(args.tail)
    val files = args1.map(readFile).toList

    setWorkingOptions(options)
    setWorkingRunMode(runMode)
  }

  def saveWorkingFiles(args: String): Unit = {
    setWorkingFiles(List(args))
  }

  def getSavedWorkingFiles(): List[String] = {
    getWorkingFiles
  }

  def parse(): String = {
    val files = getWorkingFiles
    val options = GetWorkingOptions

    (for {
      parsed      <- files.traverse(file => parser.parseFile(file)).toRight("Parsing error")
      _ = if (options.debugEnabled) then println(parsed) else ()
    } yield parsed).fold(
      leftError => leftError,
      res => {
        setParsedWorkingFile(res)
        res.toString()
      })
    }

  def typechecking(): String = {
    val parsedFiles = getParsedWorkingFile

    (for {
      typechecked <- parsedFiles.traverse(typecheck.typecheckAndLink).left.map(createTypeCheckError)
    } yield typechecked).fold(
      leftError => leftError,
      res => {
        setTypecheckedFiles(res)
        res.toString()
      }
    )
  }

  def holes(): String = {
    val parsedFiles = getParsedWorkingFile

    (for {
      typechecked <- parsedFiles.traverse(typecheck.typecheckAndLink).left.map(createTypeCheckError)
    } yield typechecked).fold(
      leftError => leftError,
      res => {
        setTypecheckedFiles(res)
        res.toString()
      }
    )
  }

  def desugaring(): String = {
    val typecheckedFiles = getTypecheckedFiles

    (for {
      desugared   <- typecheckedFiles.traverse(file => desugar.desugarProgram(file)).toRight("Desugaring error")
    } yield desugared).fold(
      leftError => leftError,
      res => {
        setDesugaredFiles(res)
        res.toString()
      }
    )
  }

  def backending(): String = {
    val desugaredFiles = getDesugaredFiles
    val runMode = getWorkingRunMode

    (for {
      result      <- desugaredFiles.traverse(expr => runBackend(expr, runMode))
    } yield result).fold(
      leftError => leftError,
      res => res.toString()
    )
  }

  def main(): String = {
    val runMode = getWorkingRunMode
    val files = getWorkingFiles
    val options = GetWorkingOptions

    (for {
      parsed      <- files.traverse(file => parser.parseFile(file)).toRight("Parsing error")
      _ = if (options.debugEnabled) then println(parsed) else ()
      typechecked <- parsed.traverse(typecheck.typecheckAndLink).left.map(createTypeCheckError)
      _ = if (options.debugEnabled) then println(typechecked) else ()
      desugared   <- typechecked.traverse(file => desugar.desugarProgram(file)).toRight("Desugaring error")
      _ = if (options.debugEnabled) then println(desugared) else ()
      result      <- desugared.traverse(expr => runBackend(expr, runMode))
    } yield result).fold(
      leftError => leftError,
      res => {
        println(files)
        res.toString()
      })
    }
  }
