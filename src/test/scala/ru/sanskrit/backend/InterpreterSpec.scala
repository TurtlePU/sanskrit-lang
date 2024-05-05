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

  it should "interpret function with function argument" in {
    val expr = Let(
        Name("dup"),
        Type.Func(Type.Func(Type.Int, Type.Int), Type.Func(Type.Int, Type.Int)),
        Abs(Name("f"), Abs(Name("x"), Let(
            Name("fRes"),
            Type.Int,
            App(Var(Name("f")), Var(Name("x"))),
            App(Var(Name("f")), Var(Name("fRes")))
        ))),
        Let(
            Name("f"),
            Type.Func(Type.Int, Type.Int),
            Abs(Name("x"), Mul(Var(Name("x")), Var(Name("x")))),
            Let(
                Name("fDup"),
                Type.Func(Type.Int, Type.Int),
                App(Var(Name("dup")), Var(Name("f"))),
                Let(
                    Name("x"),
                    Type.Int,
                    Lit(5),
                    Let(
                        Name("main"),
                        Type.Int,
                        App(Var(Name("fDup")), Var(Name("x"))),
                        Var(Name("main"))
                    )
                )
            )
        )
    )

    interpreter.run(expr) shouldBe Some(Lit(625))
  }
