package ru.sanskrit.frontend

import cats.parse.Parser
import cats.parse.Rfc5234.{sp, alpha, digit}
import ru.sanskrit.frontend.syntax.Expr

object parser:
  val exprParser = Parser.recursive[Expr] { parser =>
    val literalParser = digit.rep.map(_.foldLeft(0)((acc, a) => 10 * acc + (a - '0'))).map(Expr.Lit.apply)

    val varParser: Parser[Expr.Var] = alpha.rep.map(_.toList.mkString).map(Expr.Var.apply)

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
      } yield Expr.Lam(arg, expr)

    val simpleOrApplyParser =
      for {
        f <- simpleTermParser
        x <- (sp.rep *> simpleTermParser).rep0
      } yield x.foldLeft(f: Expr)((acc, x) => Expr.App(acc, x))

    lambdaParser | simpleOrApplyParser | literalParser
  }
