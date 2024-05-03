package ru.sanskrit.backend

import ru.sanskrit.common.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TranslatorSpec extends AnyFlatSpec with Matchers {
  "Translator" should "translate literal main as printf" in {
    val expr = Let(Name("main"), Type.Int, Lit(5), Var(Name("main")))
    val expectedTranslation = """
    |#include <stdio.h>
    |
    |
    |
    |int main() {
    |int main = 5;
    |printf("%d\n", main);
    |return 0;
    |}
    |""".stripMargin
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

    val expectedTranslation = """
    |#include <stdio.h>
    |
    |
    |typedef struct {
    |   int (*impl)(int, void*);
    |   void* env;
    |} int_int_closure;
    |
    |
    |int func_111817306(int x, void* env) {
    |return x * x;
    |}
    |
    |int main() {
    |int_int_closure f = (int_int_closure){.impl=func_111817306, .env=NULL};
    |int x = 5;
    |int main = (f.impl(x, f.env));
    |printf("%d\n", main);
    |return 0;
    |}
    |""".stripMargin

    translator.run(expr) shouldBe Some(expectedTranslation)
  }
}
