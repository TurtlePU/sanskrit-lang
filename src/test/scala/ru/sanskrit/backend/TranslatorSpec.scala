package ru.sanskrit.backend

import ru.sanskrit.common.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TranslatorSpec extends AnyFlatSpec with Matchers {
  "Translator" should "translate literal main as printf" in {
    val expr = Let(Name("main"), Type.Int, Lit(5), Var(Name("main")))

    val translationFile = scala.io.Source.fromResource("translator_literal_main.c")
    val expectedTranslation = translationFile.mkString
    translationFile.close()

    translator.run(expr) shouldBe Some(expectedTranslation)
  }

  it should "translate simple function call with closures" in {
    val expr = Let(
      Name("f"),
      Type.Func(Type.Int, Type.Int),
      Abs(Name("x"), Mul(Var(Name("x")), Var(Name("x")))),
      Let(
        Name("x"),
        Type.Int,
        Lit(5),
        Let(
          Name("main"),
          Type.Int,
          App(Var(Name("f")), Var(Name("x"))),
          Var(Name("main")) 
        )
      )
    )

    val translationFile = scala.io.Source.fromResource("translator_function_call.c")
    val expectedTranslation = translationFile.mkString
    translationFile.close()

    translator.run(expr) shouldBe Some(expectedTranslation)
  }

  it should "translate function with function argument" in {
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

    val translationFile = scala.io.Source.fromResource("translator_dup.c")
    val expectedTranslation = translationFile.mkString
    translationFile.close()

    translator.run(expr) shouldBe Some(expectedTranslation)
  }
}
