package ru.sanskrit.frontend

import cats.parse.Parser
import cats.parse.Rfc5234.{sp, alpha, digit}
import ru.sanskrit.common.Type
import ru.sanskrit.frontend.syntax.{Expr, Func, Position}

object parser:
  private def bracketParser[A](parser: Parser[A]): Parser[A] =
    for {
      _   <- Parser.string("(")
      res <- sp.rep0 *> parser <* sp.rep0
      _   <- Parser.string(")")
    } yield res

  private val typeParser = Parser.recursive[Type] { parser =>
    val intTypeParser    = Parser.string("Int").as(Type.Int)
    val simpleTypeParser = intTypeParser | bracketParser(parser)

    for {
      a     <- simpleTypeParser <* sp.rep0
      b     <- (Parser.string("->") *> sp.rep0 *> simpleTypeParser).rep0
    } yield b.foldLeft(a)((acc, x) => Type.Func(acc, x))
  }

  private val literalParser: Parser[Expr.Lit[Option]] =
    for {
      (begin, lit) <- Parser.product01(Parser.caret, digit.rep.map(_.foldLeft(0)((acc, a) => 10 * acc + (a - '0'))))
      end          <- Parser.caret
    } yield Expr.Lit(lit, Position(begin, end))

  private val varParser: Parser[Expr.Var[Option]] =
    for {
      (begin, x) <- Parser.product01(Parser.caret, alpha.rep.map(_.toList.mkString))
      end        <- Parser.caret
    } yield Expr.Var(x, None, Position(begin, end))

  val exprParser = Parser.recursive[Expr[Option]] { parser =>
    val bracketExprParser =
      for {
        (begin, res) <- Parser.product01(Parser.caret, bracketParser(parser))
        end          <- Parser.caret
      } yield res.updatePosition(begin, end)

    val simpleTermParser = varParser | bracketExprParser

    val lambdaParser =
      for {
        begin <- Parser.product01(Parser.caret, Parser.string("|") <* sp.rep0).map(_._1)
        arg   <- varParser <* sp.rep0
        _     <- Parser.string("=>") <* sp.rep0
        expr  <- parser
        end   <- Parser.caret
      } yield Expr.Lam(arg, expr, None, Position(begin, end))

    val simpleOrApplyParser =
      for {
        f <- simpleTermParser <* sp.rep0
        x <- (simpleTermParser <* sp.rep0).rep0
      } yield x.foldLeft(f: Expr[Option])((acc, x) =>
        Expr.App(acc, x, None, Position(acc.getPosition.begin, x.getPosition.end))
      )

    val mulParser =
      for {
        x  <- simpleOrApplyParser <* sp.rep0
        ys <-
          (Parser.product01(Parser.caret, Parser.string("*") *> Parser.caret).map(Position.apply) ~
            (sp.rep0 *> simpleOrApplyParser)).rep0
      } yield ys.foldLeft(x) { case (acc, (p, x) ) =>
        Expr.InfixOp(Expr.Var("*", None, p), acc, x, None, Position(acc.getPosition.begin, x.getPosition.end))
      }

    val sumParser =
      for {
        x  <- mulParser <* sp.rep0
        ys <-
          (Parser.product01(Parser.caret, Parser.string("+") *> Parser.caret).map(Position.apply) ~
            (sp.rep0 *> mulParser)).rep0
      } yield ys.foldLeft(x) { case (acc, (p, x)) =>
        Expr.InfixOp(Expr.Var("+", None, p), acc, x, None, Position(acc.getPosition.begin, x.getPosition.end))
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
    } yield Func(name.name, `type`, body, args.map { case (v, t) => v.copy(`type` = t) }*)
