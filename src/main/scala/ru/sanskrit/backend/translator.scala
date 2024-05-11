package ru.sanskrit.backend

import ru.sanskrit.common.*
import scala.collection.mutable.Map

object translator {
    type Env = Map[Name, (Type, String)]
    type Definitions = Map[String, String]

    def run(e: Expr): Option[String] =
        val builder = CBuilder()
        val env: Env = Map.empty

        builder.setBody(exprToC(e, env, builder) + "\n" + freeMemory(env))

        Some(builder.build())

    private def cleanName(n: Name) =
        n.name.replace('$', '_').replace('-', '_')

    private def isMemoizableArg(t: Type): Boolean = t == Type.Int

    private def typeToC(t: Type, builder: Option[CBuilder] = None): String = t match {
        case Type.Int => "int"
        case Type.Func(a, b) =>
            val argType = typeToC(a, builder)
            val returnType = typeToC(b, builder)
            val closureName = s"${argType}_${returnType}_closure"

            val declaration = if isMemoizableArg(a) then
                s"""typedef struct {
                |   $returnType (*impl)($argType, void*);
                |   void* env;
                |   MEMO_CACHE($returnType)* memo;
                |} $closureName;""".stripMargin
            else
                s"""typedef struct {
                |   $returnType (*impl)($argType, void*);
                |   void* env;
                |} $closureName;""".stripMargin
            
            if builder != None then
                if isMemoizableArg(a) then
                    builder.get.addDefinition(s"MEMO_CACHE($returnType)", s"DEFINE_MEMO_CACHE($returnType)")
                builder.get.addDefinition(closureName, declaration)

            closureName
    }

    private def exprToC(
        e: Expr,
        env: Env,
        builder: CBuilder,
        absType: Option[Type] = None,
        cEnv: String = "",
        prevEnvPtr: String = "NULL",
        prevEnv: List[String] = List.empty
    ): String = e match {
        case Lit(x) => x.toString

        case Var(x) => cEnv + cleanName(x)

        case Sum(a, b) =>
            val aCode = exprToC(a, env, builder, absType, cEnv, prevEnvPtr, prevEnv)
            val bCode = exprToC(b, env, builder, absType, cEnv, prevEnvPtr, prevEnv)
            s"$aCode + $bCode"

        case Mul(a, b) =>
            val aCode = exprToC(a, env, builder, absType, cEnv, prevEnvPtr, prevEnv)
            val bCode = exprToC(b, env, builder, absType, cEnv, prevEnvPtr, prevEnv)
            s"$aCode * $bCode"

        case Let(x, t, v, b) =>
            val value = exprToC(v, env, builder, Some(t), cEnv, prevEnvPtr, prevEnv)
            val definition = s"${typeToC(t, Some(builder))} ${cleanName(x)};"
            env += (x -> (t, value))
            val bodyCodeRaw = exprToC(b, env, builder, Some(t), cEnv, prevEnvPtr, prevEnv :+ definition)
            val bodyCode = b match {
                case Let(_, _, _, _) => bodyCodeRaw
                case _ if x.name == "main" => s"printf(\"%d\\n\", $bodyCodeRaw);"
                case _ => s"return $bodyCodeRaw;"
            }
            s"""${typeToC(t, Some(builder))} ${cleanName(x)} = $value;
            |$bodyCode""".stripMargin

        case Abs(x, body) =>
          val funcType = absType.get.asInstanceOf[Type.Func]
          val closureType = typeToC(funcType, Some(builder))
          val argType = typeToC(funcType.a, Some(builder))
          val returnType = typeToC(funcType.b, Some(builder))

          val newCEnv = prevEnv :+ s"$argType ${x.name};"
          val newEnv = env + (x -> (funcType.a, ""))

          val bodyCodeRaw = exprToC(body, newEnv, builder, Some(funcType.b), "env->", "env", newCEnv)
          val bodyCode = body match {
            case body: Let => bodyCodeRaw
            case _ => s"return $bodyCodeRaw;"
          }

          val implName = s"func_${bodyCode.hashCode.abs}"
          val envName = s"env_${bodyCode.hashCode.abs}"
          val memo = if isMemoizableArg(funcType.a) then s"create_memo_cache_$returnType()" else "NULL"
          val ret = if isMemoizableArg(funcType.a) then
            s"($closureType){.impl=$implName, .env=env, .memo=create_memo_cache_$returnType()}"
          else
            s"($closureType){.impl=$implName, .env=env}"

          val definition = s"""${envStructToC(newCEnv, envName)}
          |
          |$returnType $implName($argType ${x.name}, void* data) {
          |$envName* env = ($envName*)data;
          |env->${x.name} = ${x.name};
          |$bodyCode
          |}
          |
          |$closureType create_$implName(void* prevEnv, size_t prevEnvSize) {
          |$envName* env = malloc(sizeof($envName));
          |if (prevEnv != NULL) {
          |memcpy(env, prevEnv, prevEnvSize);
          |}
          |return $ret;
          |}""".stripMargin

          builder.addDefinition(implName, definition)

          if prevEnvPtr == "NULL" then
            s"create_$implName(NULL, 0)"
          else
            s"create_$implName($prevEnvPtr, sizeof(*$prevEnvPtr))"

        case App(f, x) =>
          val closureType = typeToC(env(f.x)._1, Some(builder))
          val returnType = typeToC(env(f.x)._1.asInstanceOf[Type.Func].b, Some(builder))
          if isMemoizableArg(env(f.x)._1.asInstanceOf[Type.Func].a) then
            s"memoize_$returnType($cEnv${cleanName(f.x)}.memo, $cEnv${cleanName(f.x)}.impl, ${cleanName(x.x)}, $cEnv${cleanName(f.x)}.env)"
          else
            s"$cEnv${cleanName(f.x)}.impl(${cleanName(x.x)}, $cEnv${cleanName(f.x)}.env)"
    }

    private def envStructToC(env: List[String], envName: String): String =
        s"""typedef struct {
        |${env.mkString("\n")}
        |} $envName;""".stripMargin

    private def populateCEnv(env: Env): String =
        (for {(name, (_, v)) <- env.filter(x => !x._2._2.isEmpty)} yield s"env->${cleanName(name)} = $v;").mkString("\n")

    private def freeMemory(env: Env): String =
        val closuresEnv = env.filter(x => x._2._1 match {
            case Type.Func(_, _) => true
            case _ => false
        })
        (for {(name, (t, _)) <- closuresEnv} yield if isMemoizableArg(t.asInstanceOf[Type.Func].a) then
            s"free(${cleanName(name)}.env);\nmemo_cache_free_${typeToC(t.asInstanceOf[Type.Func].b)}(${cleanName(name)}.memo);"
            else s"free(${cleanName(name)}.env);"
        ).mkString("\n")
}
