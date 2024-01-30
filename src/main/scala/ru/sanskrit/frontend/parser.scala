package ru.sanskrit.frontend

import cats.parse.Parser
import cats.parse.Rfc5234.{sp, alpha, digit}
import ru.sanskrit.frontend.syntax.Expr

object parser:
  val exprParser = Parser.recursive[Expr[Option]] { parser =>
    val literalParser: Parser[Expr.Lit[Option]] =
      digit.rep.map(_.foldLeft(0)((acc, a) => 10 * acc + (a - '0'))).map(Expr.Lit.apply)

    val varParser: Parser[Expr.Var[Option]] =
      alpha.rep.map(_.toList.mkString).map(x => Expr.Var(x, None))

    val bracketParser =
      for {
        _ <- Parser.string("(")
        res <- sp.rep0 *> parser <* sp.rep0
        _ <- Parser.string(")")
      } yield res
    val simpleTermParser = varParser | bracketParser

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
