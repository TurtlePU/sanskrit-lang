package ru.sanskrit.frontend

import cats.parse.Parser
import cats.parse.Rfc5234.{sp, alpha, digit}
import ru.sanskrit.common.Type
import ru.sanskrit.frontend.syntax.{Expr, Func}

object parser:
  private def bracketParser[A](parser: Parser[A]): Parser[A] =
    for {
      _ <- Parser.string("(")
      res <- sp.rep0 *> parser <* sp.rep0
      _ <- Parser.string(")")
    } yield res

  private val typeParser = Parser.recursive[Type] { parser =>
    val intTypeParser    = Parser.string("Int").as(Type.Int)
    val simpleTypeParser = intTypeParser | bracketParser(parser)

    for {
      a <- simpleTypeParser <* sp.rep0
      b <- (Parser.string("->") *> sp.rep0 *> simpleTypeParser).rep0
    } yield b.foldLeft(a)((acc, x) => Type.Func(acc, x))
  }

  private val literalParser: Parser[Expr.Lit[Option]] =
    for {
      lit   <- digit.rep.map(_.foldLeft(0)((acc, a) => 10 * acc + (a - '0')))
      caret <- Parser.caret
    } yield Expr.Lit(lit, caret)

  private val varParser: Parser[Expr.Var[Option]] =
    for {
      x     <- alpha.rep.map(_.toList.mkString)
      caret <- Parser.caret
    } yield Expr.Var(x, None, caret)

  val exprParser = Parser.recursive[Expr[Option]] { parser =>
    val simpleTermParser = varParser | bracketParser(parser)

    val lambdaParser =
      for {
        _ <- Parser.string("|") <* sp.rep0
        arg <- varParser <* sp.rep0
        _ <- Parser.string("=>") <* sp.rep0
        expr <- parser
      } yield Expr.Lam(arg, expr, None, expr.getCaret)

    val simpleOrApplyParser =
      for {
        f <- simpleTermParser <* sp.rep0
        x <- (simpleTermParser <* sp.rep0).rep0
      } yield x.foldLeft(f: Expr[Option])((acc, x) => Expr.App(acc, x, None, x.getCaret))

    val mulParser =
      for {
        x  <- simpleOrApplyParser <* sp.rep0
        ys <- ((Parser.string("*") *> Parser.caret) ~ (sp.rep0 *> simpleOrApplyParser)).rep0
      } yield ys.foldLeft(x) { case (acc, (c, x) ) =>
        Expr.InfixOp(Expr.Var("*", None, c), acc, x, None, x.getCaret)
      }

    val sumParser =
      for {
        x  <- mulParser <* sp.rep0
        ys <- ((Parser.string("+") *> Parser.caret) ~ (sp.rep0 *> mulParser)).rep0
      } yield ys.foldLeft(x) { case (acc, (c, x)) =>
        Expr.InfixOp(Expr.Var("+", None, c), acc, x, None, x.getCaret)
      }

    lambdaParser | sumParser | literalParser
  }

  val funcParser =
    for {
      name <- varParser <* sp.rep0
      args <- (
        (Parser.string("(") *> sp.rep0 *> varParser <* sp.rep0) ~
          (Parser.string(":") *> sp.rep0 *> typeParser.? <* sp.rep0 <* Parser.string(")") <* sp.rep0)
      ).rep0
      `type` <- (Parser.string(":") *> sp.rep0 *> typeParser <* sp.rep0).backtrack.?
      _      <- Parser.string(":=") <* sp.rep0
      body   <- exprParser
    } yield Func(name.name, `type`, body, args.map { case (v, t) => v.copy(`type` = t) }: _*)
