package ru.sanskrit.frontend

import cats.parse.{Parser, Parser0}
import cats.parse.Rfc5234.{sp, alpha, digit, lf, vchar, wsp}
import ru.sanskrit.common.Type
import ru.sanskrit.frontend.syntax.{Expr, Func, Position}

object parser:
  extension [A](a: Parser0[A])
    def !*[B](b: Parser[B]): Parser[(A, B)] = Parser.product01(a, b)
    def !<[B](b: Parser[B]): Parser[A]      = (a !* b).map(_._1)
    def !>[B](b: Parser[B]): Parser[B]      = (a !* b).map(_._2)

  extension [A](a: Parser[A])
    def !? : Parser0[Option[A]]  = a.map(Some.apply).backtrack.orElse(Parser.pure(None))
    def repAll: Parser0[List[A]] =
      a.!?.flatMap(_.fold(Parser.unit.as(List.empty))(res1 => a.repAll.map(res2 => res1 :: res2)))

  private val comment: Parser[Unit] = (Parser.char('#') *> (vchar | wsp).rep0).void
  private def space(basicSpace: Parser[Unit]): Parser0[Unit] =
    (basicSpace.rep0 *> (comment <* basicSpace.rep0).repAll <* basicSpace.rep0).void
  private val exprSpace: Parser0[Unit] = space(wsp)
  private val funcSpace: Parser0[Unit] = space(wsp | lf | Parser.char(13.toChar))

  private def bracketParser[A](parser: Parser[A]): Parser[A] =
    for {
      _   <- Parser.char('(')
      res <- exprSpace *> parser <* exprSpace
      _   <- Parser.char(')')
    } yield res

  private val typeParser = Parser.recursive[Type] { parser =>
    val intTypeParser    = Parser.string("Int").as(Type.Int)
    val simpleTypeParser = intTypeParser | bracketParser(parser)

    for {
      a     <- simpleTypeParser <* exprSpace
      b     <- (Parser.string("->") *> exprSpace *> simpleTypeParser).rep0
    } yield b.foldLeft(a)((acc, x) => Type.Func(acc, x))
  }

  private val literalParser: Parser[Expr.Lit[Option]] =
    for {
      (begin, lit) <- Parser.caret !*
        (Parser.char('-').? !*
          digit.rep.map(_.foldLeft(0)((acc, a) => 10 * acc + (a - '0')))).map((a, b) => a.fold(b)(_ => -b))
      end          <- Parser.caret
    } yield Expr.Lit(lit, Position(begin, end))

  private val varParser: Parser[Expr.Var[Option]] =
    for {
      (begin, x) <- Parser.caret !*
        (alpha ~ (alpha | digit | Parser.charIn("!@#$%^&\'\";")).rep0).map { case (a, b) => (a :: b).mkString }
      end        <- Parser.caret
    } yield Expr.Var(x, None, Position(begin, end))

  private val holeParser: Parser[Expr.Hole[Option]] =
    for {
      begin <- Parser.caret !< Parser.char('_')
      end   <- Parser.caret
    } yield Expr.Hole(None, Position(begin, end))

  val exprParser = Parser.recursive[Expr[Option]] { parser =>
    val bracketExprParser =
      for {
        (begin, res) <- Parser.caret !* bracketParser(parser)
        end          <- Parser.caret
      } yield res.updatePosition (begin, end)

    val lambdaParser =
      for {
        begin <- (Parser.caret !< Parser.string("|") <* exprSpace)
        arg   <- varParser <* exprSpace
        _     <- Parser.string("=>") <* exprSpace
        expr  <- parser
        end   <- Parser.caret
      } yield Expr.Lam(arg, expr, None, Position(begin, end))

    val simpleTermParser = varParser | bracketExprParser | literalParser | lambdaParser | holeParser

    val simpleOrApplyParser =
      for {
        f <- simpleTermParser <* exprSpace
        x <- (simpleTermParser <* exprSpace).repAll
      } yield x.foldLeft(f: Expr[Option])((acc, x) =>
        Expr.App(acc, x, None, Position(acc.getPosition.begin, x.getPosition.end))
      )

    val mulParser =
      for {
        x  <- simpleOrApplyParser <* exprSpace
        ys <-
          ((Parser.caret !* Parser.string("*") *> Parser.caret).map(Position.apply) ~
            (exprSpace *> simpleOrApplyParser)).rep0
      } yield ys.foldLeft(x) { case (acc, (p, x) ) =>
        Expr.InfixOp(Expr.Var("*", None, p), acc, x, None, Position(acc.getPosition.begin, x.getPosition.end))
      }

    val sumParser =
      for {
        x  <- mulParser <* exprSpace
        ys <-
          ((Parser.caret !* Parser.string("+") *> Parser.caret).map(Position.apply) ~
            (exprSpace *> mulParser)).rep0
      } yield ys.foldLeft(x) { case (acc, (p, x)) =>
        Expr.InfixOp(Expr.Var("+", None, p), acc, x, None, Position(acc.getPosition.begin, x.getPosition.end))
      }

    exprSpace !> sumParser
  }

  private val argParser =
    ((Parser.string("(") *> funcSpace *> varParser <* funcSpace) ~
      (Parser.string(":") *> funcSpace *> typeParser <* funcSpace <* Parser.string(")")))
      .map { case (v, t) => v.copy(`type` = Some(t))} | varParser


  val funcParser =
    for {
      name <- funcSpace !> varParser <* funcSpace
      args <- (argParser <* funcSpace).rep0
      `type` <- (Parser.string(":") *> funcSpace *> typeParser <* funcSpace).backtrack.?
      _      <- Parser.string(":=") <* funcSpace
      body   <- exprParser
    } yield Func(name.name, `type`, body, args*)


  def parseFile(file: String): Option[List[Func[Option]]] =
    (funcParser.repAll <* funcSpace).parseAll(file).toOption
