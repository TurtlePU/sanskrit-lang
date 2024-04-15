package ru.sanskrit.backend

import ru.sanskrit.common.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InterpreterSpec extends AnyFlatSpec with Matchers:
  "Interpreter" should "interpret literal as itself" in {
    interpreter.run(Lit(10)) shouldBe Some(Lit(10))
  }

  it should "interpret sum" in {
    val expr = Let(
      Name("a"),
      Type.Int,
      Lit(2),
      Let(
        Name("b"),
        Type.Int,
        Lit(3),
        Let(
          Name("res"),
          Type.Int,
          Sum(Var(Name("a")), Var(Name("b"))),
          Var(Name("res"))
        )
      )
    )
    interpreter.run(expr) shouldBe Some(Lit(5))
  }

  it should "interpret mul" in {
    val expr = Let(
      Name("a"),
      Type.Int,
      Lit(2),
      Let(
        Name("b"),
        Type.Int,
        Lit(3),
        Let(
          Name("res"),
          Type.Int,
          Mul(Var(Name("a")), Var(Name("b"))),
          Var(Name("res"))
        )
      )
    )
    interpreter.run(expr) shouldBe Some(Lit(6))
  }

  it should "interpret function" in {
    val expr = Let(
      Name("square"),
      Type.Func(Type.Int, Type.Int),
      Abs(Name("x"), Mul(Var(Name("x")), Var(Name("x")))),
      Let(
        Name("y"),
        Type.Int,
        Lit(9),
        App(Var(Name("square")), Var(Name("y")))
      )
    )
    interpreter.run(expr) shouldBe Some(Lit(81))
  }
