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
    digit.rep.map(_.foldLeft(0)((acc, a) => 10 * acc + (a - '0'))).map(Expr.Lit.apply)

  private val varParser: Parser[Expr.Var[Option]] =
    alpha.rep.map(_.toList.mkString).map(x => Expr.Var(x, None))

  val exprParser = Parser.recursive[Expr[Option]] { parser =>
    val simpleTermParser = varParser | bracketParser(parser)

    val lambdaParser =
      for {
        _ <- Parser.string("|") <* sp.rep0
        arg <- varParser <* sp.rep0
        _ <- Parser.string("=>") <* sp.rep0
        expr <- parser
      } yield Expr.Lam(arg, expr, None)

    val simpleOrApplyParser =
      for {
        f <- simpleTermParser
        x <- (sp.rep *> simpleTermParser).rep0
      } yield x.foldLeft(f: Expr[Option])((acc, x) => Expr.App(acc, x, None))

    lambdaParser | simpleOrApplyParser | literalParser
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
