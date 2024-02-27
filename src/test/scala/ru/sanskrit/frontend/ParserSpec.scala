package ru.sanskrit.frontend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.sanskrit.common.Type
import ru.sanskrit.frontend.syntax.{Expr, Func}

class ParserSpec extends AnyFlatSpec with Matchers:
  "Expr" should "parse variable names" in {
    parser.exprParser.parse("aboba") shouldBe Right(("", Expr.Var("aboba", None)))
  }

  it should "parse int literals" in {
    parser.exprParser.parse("42") shouldBe Right(("", Expr.Lit(42)))
  }

  it should "parse application" in {
    parser.exprParser.parse("f x") shouldBe Right(("", Expr.App(Expr.Var("f", None), Expr.Var("x", None), None)))
  }

  it should "parse carried application" in {
    parser.exprParser.parse("f x y z") shouldBe
      Right(("",
        Expr.App(
          Expr.App(Expr.App(Expr.Var("f", None), Expr.Var("x", None), None), Expr.Var("y", None), None),
          Expr.Var("z", None),
          None
        )
      ))
  }

  it should "parse sum" in {
    parser.exprParser.parse("a + b") shouldBe
      Right(("", Expr.InfixOp(Expr.Var("+", None), Expr.Var("a", None), Expr.Var("b", None), None)))
  }

  it should "parse mul" in {
    parser.exprParser.parse("a * b") shouldBe
      Right(("", Expr.InfixOp(Expr.Var("*", None), Expr.Var("a", None), Expr.Var("b", None), None)))
  }

  "Func" should "parse constant let" in {
    parser.funcParser.parse("a: Int := 42") shouldBe
      Right(("", Func("a", Some(Type.Int), Expr.Lit(42))))
  }

  it should "parse id function" in {
    parser.funcParser.parse("id (x: Int): Int := x") shouldBe
      Right(("", Func("id", Some(Type.Int), Expr.Var("x", None), Expr.Var("x", Some(Type.Int)))))
  }

  it should "parse short id function" in {
    parser.funcParser.parse("id (x: Int) := x") shouldBe
      Right(("", Func("id", None, Expr.Var("x", None), Expr.Var("x", Some(Type.Int)))))
  }
