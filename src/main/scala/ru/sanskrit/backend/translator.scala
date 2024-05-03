package ru.sanskrit.backend

import ru.sanskrit.common.*
import scala.collection.mutable.Map

object translator {
    type Closures = Map[Type, String]
    type TypeEnv = Map[Name, Type]
    type Definitions = Map[String, String]

    def run(e: Expr): Option[String] =
        val closures: Closures = Map.empty
        val definitions: Definitions = Map.empty
        val body = exprToC(e, closures, Map.empty, definitions)

        val closuresCode = closures.values.mkString
        val definitionsCode = definitions.values.mkString

        Some(s"""
        |#include <stdio.h>
        |
        |$closuresCode
        |$definitionsCode
        |int main() {
        |$body
        |return 0;
        |}
        |""".stripMargin)

    private def cleanName(n: Name) =
        n.name.replace('$', '_').replace('-', '_')

    private def typeToC(t: Type, closures: Closures): String = t match {
        case Type.Int => "int"
        case Type.Func(a, b) =>
            val argType = typeToC(a, closures)
            val returnType = typeToC(b, closures)
            val closureName = s"${argType}_${returnType}_closure"
            val declaration = s"""
            |typedef struct {
            |   $returnType (*impl)($argType, void*);
            |   void* env;
            |} $closureName;
            |""".stripMargin
            closures += (Type.Func(a, b) -> declaration)
            closureName
    }

    private def exprToC(e: Expr, closures: Closures, env: TypeEnv, definitions: Definitions, absType: Option[Type] = None): String = e match {
        case Lit(x) => x.toString
        case Var(x) => cleanName(x)
        case Sum(a, b) => s"${exprToC(a, closures, env, definitions)} + ${exprToC(b, closures, env, definitions)}"
        case Mul(a, b) => s"${exprToC(a, closures, env, definitions)} * ${exprToC(b, closures, env, definitions)}"
        case Let(x, t, v, b) =>
            val value = exprToC(v, closures, env, definitions, Some(t))
            env += (x -> t)
            val bodyCode = b match {
                case v: Var => s"printf(\"%d\\n\", ${exprToC(b, closures, env, definitions, Some(t))});"
                case _ => exprToC(b, closures, env, definitions, Some(t))
            }
            s"""${typeToC(t, closures)} ${cleanName(x)} = $value;
            |$bodyCode""".stripMargin
        case Abs(x, body) =>
          val funcType = absType.get.asInstanceOf[Type.Func]
          val closureType = typeToC(funcType, closures)
          val argType = typeToC(funcType.a, closures)
          val returnType = typeToC(funcType.b, closures)
          val bodyCode = exprToC(body, closures, env + (x -> funcType.a), definitions)
          val implName = s"func_${bodyCode.hashCode.abs}"
          definitions += (implName -> s"""
          |$returnType $implName($argType ${x.name}, void* env) {
          |return $bodyCode;
          |}
          |""".stripMargin)
          s"($closureType){.impl=$implName, .env=NULL}"
        case App(f, x) =>
          val closureType = typeToC(env(f.x), closures)
          s"(${cleanName(f.x)}.impl(${cleanName(x.x)}, ${cleanName(f.x)}.env))"
    }
}
