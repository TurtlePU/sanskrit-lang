package ru.sanskrit.backend

import ru.sanskrit.common.*
import scala.collection.mutable.Map

object translator {
    type Closures = Map[Type, String]
    type Env = Map[Name, (Type, String)]
    type Definitions = Map[String, String]

    def run(e: Expr): Option[String] =
        val closures: Closures = Map.empty
        val definitions: Definitions = Map.empty
        val env: Env = Map.empty
        val body = exprToC(e, closures, env, definitions)

        val closuresCode = closures.values.mkString("\n")
        val definitionsCode = definitions.values.mkString("\n")
        val globalScope = List(closuresCode, definitionsCode).filter(x => !x.isEmpty).mkString("\n\n")

        Some(s"""#include <stdio.h>
        |#include <stdlib.h>
        |
        |$globalScope
        |
        |int main() {
        |$body
        |${freeMemory(env)}
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
            val declaration = s"""typedef struct {
            |   $returnType (*impl)($argType, void*);
            |   void* env;
            |} $closureName;""".stripMargin
            closures += (Type.Func(a, b) -> declaration)
            closureName
    }

    private def exprToC(
        e: Expr,
        closures: Closures,
        env: Env,
        definitions: Definitions,
        absType: Option[Type] = None,
        cEnv: String = ""
    ): String = e match {
        case Lit(x) => x.toString
        case Var(x) => cEnv + cleanName(x)
        case Sum(a, b) =>
            val aCode = exprToC(a, closures, env, definitions, absType, cEnv)
            val bCode = exprToC(b, closures, env, definitions, absType, cEnv)
            s"$aCode + $bCode"
        case Mul(a, b) =>
            val aCode = exprToC(a, closures, env, definitions, absType, cEnv)
            val bCode = exprToC(b, closures, env, definitions, absType, cEnv)
            s"$aCode * $bCode"
        case Let(x, t, v, b) =>
            val value = exprToC(v, closures, env, definitions, absType=Some(t))
            env += (x -> (t, value))
            val bodyCode = b match {
                case v: Var => s"printf(\"%d\\n\", ${exprToC(b, closures, env, definitions, absType=Some(t))});"
                case _ => exprToC(b, closures, env, definitions, absType=Some(t))
            }
            s"""${typeToC(t, closures)} ${cleanName(x)} = $value;
            |$bodyCode""".stripMargin
        case Abs(x, body) =>
          val funcType = absType.get.asInstanceOf[Type.Func]
          val closureType = typeToC(funcType, closures)
          val argType = typeToC(funcType.a, closures)
          val returnType = typeToC(funcType.b, closures)
          val bodyCode = exprToC(body, closures, env, definitions, cEnv="env->")
          val implName = s"func_${bodyCode.hashCode.abs}"
          val envName = s"env_${bodyCode.hashCode.abs}"
          definitions += (implName -> s"""${envStructToC(env + (x -> (funcType.a, "")), envName, closures)}
          |
          |$returnType $implName($argType ${x.name}, void* data) {
          |$envName* env = ($envName*)data;
          |env->${x.name} = ${x.name};
          |return $bodyCode;
          |}
          |
          |$closureType create_$implName() {
          |$envName* env = malloc(sizeof($envName));
          |${populateCEnv(env)}
          |return ($closureType){.impl=$implName, .env=env};
          |}""".stripMargin)
          s"create_$implName()"
        case App(f, x) =>
          val closureType = typeToC(env(f.x)._1, closures)
          s"(${cleanName(f.x)}.impl(${cleanName(x.x)}, ${cleanName(f.x)}.env))"
    }

    private def envStructToC(env: Env, envName: String, closures: Closures): String =
        val variables = (for {(name, (t, _)) <- env} yield s"${typeToC(t, closures)} ${cleanName(name)};").mkString("\n")
        s"""typedef struct {
        |$variables
        |} $envName;""".stripMargin

    private def populateCEnv(env: Env): String =
        (for {(name, (_, v)) <- env} yield s"env->${cleanName(name)} = $v;").mkString("\n")

    private def freeMemory(env: Env): String =
        val closuresEnv = env.filter(x => x._2._1 match {
            case Type.Func(_, _) => true
            case _ => false
        })
        (for {(name, (t, _)) <- closuresEnv} yield s"free(${cleanName(name)}.env);").mkString("\n")
}
