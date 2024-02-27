package ru.sanskrit.backend

import ru.sanskrit.common.{Expr, Name, Rhs, Type}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InterpreterSpec extends AnyFlatSpec with Matchers:
  "Interpreter" should "interpret literal as itself" in {
    interpreter.run(Expr.Val.Lit(10)) shouldBe Some(Rhs.Val(Expr.Val.Lit(10)))
  }

  it should "interpret sum" in {
    val expr = Expr.Let(
      Name("a"),
      Type.Int,
      Rhs.Val(Expr.Val.Lit(2)),
      Expr.Let(
        Name("b"),
        Type.Int,
        Rhs.Val(Expr.Val.Lit(3)),
        Expr.Let(
          Name("res"),
          Type.Int,
          Rhs.Sum(Expr.Val.Var(Name("a")), Expr.Val.Var(Name("b"))),
          Expr.Val.Var(Name("res"))
        )
      )
    )
    interpreter.run(expr) shouldBe Some(Rhs.Val(Expr.Val.Lit(5)))
  }

  it should "interpret mul" in {
    val expr = Expr.Let(
      Name("a"),
      Type.Int,
      Rhs.Val(Expr.Val.Lit(2)),
      Expr.Let(
        Name("b"),
        Type.Int,
        Rhs.Val(Expr.Val.Lit(3)),
        Expr.Let(
          Name("res"),
          Type.Int,
          Rhs.Mul(Expr.Val.Var(Name("a")), Expr.Val.Var(Name("b"))),
          Expr.Val.Var(Name("res"))
        )
      )
    )
    interpreter.run(expr) shouldBe Some(Rhs.Val(Expr.Val.Lit(6)))
  }

  it should "interpret function" in {
    val xName: Name = Name("x")
    val funcName: Name = Name("square")
    val implName: Name = Name("squareImpl")
    val expr = Expr.Let(
      funcName,
      Type.Func(Type.Int, Type.Int),
      Rhs.Abs(xName, Expr.Let(
        implName,
        Type.Int,
        Rhs.Mul(Expr.Val.Var(xName), Expr.Val.Var(xName)),
        Expr.Val.Var(implName)
      )),
      Expr.Let(
        Name("res"),
        Type.Int,
        Rhs.App(Expr.Val.Var(funcName), Expr.Val.Lit(9)),
        Expr.Val.Var(Name("res"))
      )
    )
    interpreter.run(expr) shouldBe Some(Rhs.Val(Expr.Val.Lit(81)))
  }
