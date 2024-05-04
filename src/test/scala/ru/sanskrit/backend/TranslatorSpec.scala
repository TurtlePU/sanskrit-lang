package ru.sanskrit.backend

import ru.sanskrit.common.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TranslatorSpec extends AnyFlatSpec with Matchers {
  "Translator" should "translate literal main as printf" in {
    val expr = Let(Name("main"), Type.Int, Lit(5), Var(Name("main")))
    val expectedTranslation = """#include <stdio.h>
    |#include <stdlib.h>
    |
    |
    |
    |int main() {
    |int main = 5;
    |printf("%d\n", main);
    |
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

    val expectedTranslation = """#include <stdio.h>
    |#include <stdlib.h>
    |
    |typedef struct {
    |   int (*impl)(int, void*);
    |   void* env;
    |} int_int_closure;
    |
    |typedef struct {
    |int x;
    |} env_1076984342;
    |
    |int func_1076984342(int x, void* data) {
    |env_1076984342* env = (env_1076984342*)data;
    |env->x = x;
    |return env->x * env->x;
    |}
    |
    |int_int_closure create_func_1076984342() {
    |env_1076984342* env = malloc(sizeof(env_1076984342));
    |
    |return (int_int_closure){.impl=func_1076984342, .env=env};
    |}
    |
    |int main() {
    |int_int_closure f = create_func_1076984342();
    |int x = 5;
    |int main = (f.impl(x, f.env));
    |printf("%d\n", main);
    |free(f.env);
    |return 0;
    |}
    |""".stripMargin

    translator.run(expr) shouldBe Some(expectedTranslation)
  }
}
