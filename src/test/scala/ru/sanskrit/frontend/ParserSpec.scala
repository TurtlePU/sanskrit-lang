package ru.sanskrit.frontend

import cats.parse.Caret
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.sanskrit.common.Type
import ru.sanskrit.frontend.syntax.{Expr, Func, Position}

class ParserSpec extends AnyFlatSpec with Matchers:
  "Expr" should "parse variable names" in {
    parser.exprParser.parse("aboba") shouldBe
      Right(("", Expr.Var("aboba", None, Position(Caret(0, 0, 0), Caret(0, 5, 5)))))
  }

  it should "parse int literals" in {
    parser.exprParser.parse("42") shouldBe Right(("", Expr.Lit(42, Position(Caret(0, 0, 0), Caret(0, 2, 2)))))
  }

  it should "parse application" in {
    parser.exprParser.parse("f x") shouldBe
      Right(("",
        Expr.App(
          Expr.Var("f", None, Position(Caret(0, 0, 0), Caret(0, 1, 1))),
          Expr.Var("x", None, Position(Caret(0, 2, 2), Caret(0, 3, 3))),
          None,
          Position(Caret(0, 0, 0), Caret(0, 3, 3))
        )
      ))
  }

  it should "parse carried application" in {
    parser.exprParser.parse("f x y z") shouldBe
      Right(("",
        Expr.App(
          Expr.App(
            Expr.App(
              Expr.Var("f", None, Position(Caret(0, 0, 0), Caret(0, 1, 1))),
              Expr.Var("x", None, Position(Caret(0, 2, 2), Caret(0, 3, 3))),
              None,
              Position(Caret(0, 0, 0), Caret(0, 3, 3))
            ),
            Expr.Var("y", None, Position(Caret(0, 4, 4), Caret(0, 5, 5))),
            None,
            Position(Caret(0, 0, 0), Caret(0, 5, 5))
          ),
          Expr.Var("z", None, Position(Caret(0, 6, 6), Caret(0, 7, 7))),
          None,
          Position(Caret(0, 0, 0), Caret(0, 7, 7))
        )
      ))
  }

  it should "parse sum" in {
    parser.exprParser.parse("a + b") shouldBe
      Right(("",
        Expr.InfixOp(
          Expr.Var("+", None, Position(Caret(0, 2, 2), Caret(0, 3, 3))),
          Expr.Var("a", None, Position(Caret(0, 0, 0), Caret(0, 1, 1))),
          Expr.Var("b", None, Position(Caret(0, 4, 4), Caret(0, 5, 5))),
          None,
          Position(Caret(0, 0, 0), Caret(0, 5, 5))
        )
      ))
  }

  it should "parse mul" in {
    parser.exprParser.parse("a * b") shouldBe
      Right(("",
        Expr.InfixOp(
          Expr.Var("*", None, Position(Caret(0, 2, 2), Caret(0, 3, 3))),
          Expr.Var("a", None, Position(Caret(0, 0, 0), Caret(0, 1, 1))),
          Expr.Var("b", None, Position(Caret(0, 4, 4), Caret(0, 5, 5))),
          None,
          Position(Caret(0, 0, 0), Caret(0, 5, 5))
        )
      ))
  }

  it should "parse complex expr" in {
    parser.exprParser.parse("a + b * (c + d) + e") shouldBe
      Right(("", Expr.InfixOp(
        Expr.Var("+", None, Position(Caret(0, 16, 16), Caret(0, 17, 17))),
        Expr.InfixOp(
          Expr.Var("+", None, Position(Caret(0, 2, 2), Caret(0, 3, 3))),
          Expr.Var("a", None, Position(Caret(0, 0, 0), Caret(0, 1, 1))),
          Expr.InfixOp(
            Expr.Var("*", None, Position(Caret(0, 6, 6), Caret(0, 7, 7))),
            Expr.Var("b", None, Position(Caret(0, 4, 4), Caret(0, 5, 5))),
            Expr.InfixOp(
              Expr.Var("+", None, Position(Caret(0, 11, 11), Caret(0, 12, 12))),
              Expr.Var("c", None, Position(Caret(0, 9, 9), Caret(0, 10, 10))),
              Expr.Var("d", None, Position(Caret(0, 13, 13), Caret(0, 14, 14))),
              None,
              Position(Caret(0, 8, 8), Caret(0, 15, 15))
            ),
            None,
            Position(Caret(0, 4, 4), Caret(0, 15, 15))
          ),
          None,
          Position(Caret(0, 0, 0), Caret(0, 15, 15))
        ),
        Expr.Var("e", None, Position(Caret(0, 18, 18), Caret(0, 19, 19))),
        None,
        Position(Caret(0, 0, 0), Caret(0, 19, 19))
      )))
  }

  it should "parse typed holes" in {
    parser.exprParser.parse("a * _") shouldBe
      Right("", Expr.InfixOp(
        Expr.Var("*", None, Position(Caret(0, 2, 2), Caret(0, 3, 3))),
        Expr.Var("a", None, Position(Caret(0, 0, 0), Caret(0, 1, 1))),
        Expr.Hole(None, Position(Caret(0, 4, 4), Caret(0, 5, 5))),
        None,
        Position(Caret(0, 0, 0), Caret(0, 5, 5))
      ))
  }

  "Func" should "parse constant let" in {
    parser.funcParser.parse("a: Int := 42") shouldBe
      Right(("", Func("a", Some(Type.Int), Expr.Lit(42, Position(Caret(0, 10, 10), Caret(0, 12, 12))))))
  }

  it should "parse id function" in {
    parser.funcParser.parse("id (x: Int): Int := x") shouldBe
      Right(("",
        Func(
          "id",
          Some(Type.Int),
          Expr.Var("x", None, Position(Caret(0, 20, 20), Caret(0, 21, 21))),
          Expr.Var("x", Some(Type.Int), Position(Caret(0, 4, 4), Caret(0, 5, 5)))
        )
      ))
  }

  it should "parse short id function" in {
    parser.funcParser.parse("id (x: Int) := x") shouldBe
      Right(("",
        Func(
          "id",
          None,
          Expr.Var("x", None, Position(Caret(0, 15, 15), Caret(0, 16, 16))),
          Expr.Var("x", Some(Type.Int), Position(Caret(0, 4, 4), Caret(0, 5, 5)))
        )
      ))
  }
