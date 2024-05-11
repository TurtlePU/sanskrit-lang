package ru.sanskrit.backend

import scala.collection.mutable.{ListBuffer, Set}

class CBuilder {
  var includes: String = "#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>"
  var definitions: ListBuffer[String] = ListBuffer.empty
  var mainBody: String = ""

  var definitionsSet: Set[String] = Set.empty

  def build(): String =
    List(includes, definitions.mkString("\n\n"), mainBody).mkString("\n\n")

  def addDefinition(name: String, definition: String): Unit =
    if !definitionsSet.contains(name) then
        definitionsSet += name
        definitions += definition

  def setBody(body: String): Unit =
    mainBody = s"""int main() {
    |$body
    |return 0;
    |}
    |""".stripMargin
}
