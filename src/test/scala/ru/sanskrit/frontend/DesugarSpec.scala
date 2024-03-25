package ru.sanskrit.frontend

import cats.parse.Caret
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.sanskrit.common.{App, Abs, Expr, Let, Lit, Mul, Name, Sum, Type, Var}
import ru.sanskrit.frontend.syntax.{Expr => FExpr, Func}

class DesugarSpec extends AnyFlatSpec with Matchers:
  private val testCaret = Caret(0, 0, 0)

  private def shrinkVarName(name: Name): Name = Name(name.name.split("\\$").head)

  private def cutVar(v: Var): Var = Var(shrinkVarName(v.x))

  private def cutUUID(expr: Expr): Expr = expr match {
    case Let(name, t, v, b) => Let(shrinkVarName(name), t, cutUUID(v), cutUUID(b))
    case l: Lit             => l
    case v: Var             => cutVar(v)
    case Abs(name, t)       => Abs(shrinkVarName(name), cutUUID(t))
    case App(f, x)          => App(cutVar(f), cutVar(x))
    case Sum(a, b)          => Sum(cutVar(a), cutVar(b))
    case Mul(a, b)          => Mul(cutVar(a), cutVar(b))
  }

  private def updateDesugaredExpr(res: (Expr, List[(Name, Type, Expr)])): (Expr, List[(Name, Type, Expr)]) =
    (cutUUID(res._1), res._2.map { case (n, t, e) => (shrinkVarName(n), t, cutUUID(e)) })

  "desugarExpr" should "desugar literal" in {
    desugar.desugarExpr(FExpr.Lit(42, testCaret)) shouldBe Some(Lit(42), List.empty)
  }

  it should "desugar variable name" in {
    desugar.desugarExpr(FExpr.Var("test", Type.Int, testCaret)) shouldBe Some(Var(Name("test")), List.empty)
  }

  it should "desugar application" in {
    desugar.desugarExpr(
      FExpr.App(
        FExpr.Var("f", Type.Func(Type.Int, Type.Int), testCaret),
        FExpr.Var("x", Type.Int, testCaret),
        Type.Int,
        testCaret
      )
    ).map(updateDesugaredExpr) shouldBe
      Some(
        App(Var(Name("f")), Var(Name("a"))),
        List((Name("f"), Type.Func(Type.Int, Type.Int), Var(Name("f"))), (Name("a"), Type.Int, Var(Name("x"))))
      )
  }

  it should "desugar lambda" in {
    desugar.desugarExpr(
      FExpr.Lam(FExpr.Var("x", Type.Int, testCaret), FExpr.Lit(42, testCaret), Type.Func(Type.Int, Type.Int), testCaret)
    ).map(updateDesugaredExpr) shouldBe
      Some(Abs(Name("x"), Let(Name("f"), Type.Int, Lit(42), Var(Name("f")))), List.empty)
  }

  "desugarProgram" should "desugar simple program" in {
    desugar.desugarProgram(List(Func("main", Type.Int, FExpr.Lit(42, testCaret)))) shouldBe
      Some(Let(Name("main"), Type.Int, Lit(42), Var(Name("main"))))
  }

  it should "fail on program without main" in {
    desugar.desugarProgram(List.empty) shouldBe None
  }
