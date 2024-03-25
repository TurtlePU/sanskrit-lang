package ru.sanskrit.frontend

import cats.parse.Caret
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.sanskrit.common.Type
import ru.sanskrit.frontend.syntax.{Expr, Func}

class ParserSpec extends AnyFlatSpec with Matchers:
  "Expr" should "parse variable names" in {
    parser.exprParser.parse("aboba") shouldBe Right(("", Expr.Var("aboba", None, Caret(0, 5, 5))))
  }

  it should "parse int literals" in {
    parser.exprParser.parse("42") shouldBe Right(("", Expr.Lit(42, Caret(0, 2, 2))))
  }

  it should "parse application" in {
    parser.exprParser.parse("f x") shouldBe
      Right(("",
        Expr.App(Expr.Var("f", None, Caret(0, 1, 1)), Expr.Var("x", None, Caret(0, 3, 3)), None, Caret(0, 3, 3))
      ))
  }

  it should "parse carried application" in {
    parser.exprParser.parse("f x y z") shouldBe
      Right(("",
        Expr.App(
          Expr.App(
            Expr.App(Expr.Var("f", None, Caret(0, 1, 1)), Expr.Var("x", None, Caret(0, 3, 3)), None, Caret(0, 3, 3)),
            Expr.Var("y", None, Caret(0, 5, 5)),
            None,
            Caret(0, 5, 5)
          ),
          Expr.Var("z", None, Caret(0, 7, 7)),
          None,
          Caret(0, 7, 7)
        )
      ))
  }

  it should "parse sum" in {
    parser.exprParser.parse("a + b") shouldBe
      Right(("",
        Expr.InfixOp(
          Expr.Var("+", None, Caret(0, 3, 3)),
          Expr.Var("a", None, Caret(0, 1, 1)),
          Expr.Var("b", None, Caret(0, 5, 5)),
          None,
          Caret(0, 5, 5)
        )
      ))
  }

  it should "parse mul" in {
    parser.exprParser.parse("a * b") shouldBe
      Right(("",
        Expr.InfixOp(
          Expr.Var("*", None, Caret(0, 3, 3)),
          Expr.Var("a", None, Caret(0, 1, 1)),
          Expr.Var("b", None, Caret(0, 5, 5)),
          None,
          Caret(0, 5, 5)
        )
      ))
  }

  it should "parse complex expr" in {
    parser.exprParser.parse("a + b * (c + d) + e") shouldBe
      Right(("", Expr.InfixOp(
        Expr.Var("+", None, Caret(0, 17, 17)),
        Expr.InfixOp(
          Expr.Var("+", None, Caret(0, 3, 3)),
          Expr.Var("a", None, Caret(0, 1, 1)),
          Expr.InfixOp(
            Expr.Var("*", None, Caret(0, 7, 7)),
            Expr.Var("b", None, Caret(0, 5, 5)),
            Expr.InfixOp(
              Expr.Var("+", None, Caret(0, 12, 12)),
              Expr.Var("c", None, Caret(0, 10, 10)),
              Expr.Var("d", None, Caret(0, 14, 14)),
              None,
              Caret(0, 14, 14)
            ),
            None,
            Caret(0, 14, 14)
          ),
          None,
          Caret(0, 14, 14)
        ),
        Expr.Var("e", None, Caret(0, 19, 19)),
        None,
        Caret(0, 19, 19)
      )))
  }

  "Func" should "parse constant let" in {
    parser.funcParser.parse("a: Int := 42") shouldBe
      Right(("", Func("a", Some(Type.Int), Expr.Lit(42, Caret(0, 12, 12)))))
  }

  it should "parse id function" in {
    parser.funcParser.parse("id (x: Int): Int := x") shouldBe
      Right(("",
        Func("id", Some(Type.Int), Expr.Var("x", None, Caret(0, 21, 21)), Expr.Var("x", Some(Type.Int), Caret(0, 5, 5)))
      ))
  }

  it should "parse short id function" in {
    parser.funcParser.parse("id (x: Int) := x") shouldBe
      Right(("",
        Func("id", None, Expr.Var("x", None, Caret(0, 16, 16)), Expr.Var("x", Some(Type.Int), Caret(0, 5, 5)))
      ))
  }
