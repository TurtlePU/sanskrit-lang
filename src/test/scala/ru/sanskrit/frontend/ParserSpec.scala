package ru.sanskrit.frontend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.sanskrit.frontend.syntax.Expr

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
