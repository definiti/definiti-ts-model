package definiti.tsmodel.generator

import definiti.tsmodel.TsAST._
import definiti.tsmodel.utils.StringUtils

object CodeGenerator extends CodeGenerator

trait CodeGenerator extends ExpressionGenerator {
  val noIndent = ""
  def inc(current: String): String = s"${current}  "
  def quote(value: String): String = s"'${value.replaceAllLiterally("\'", "\\\'")}'"

  def generate(module: Module): String = {
    module.statements.map(generateStatement(_, noIndent)).mkString("\n")
  }

  private def generateStatement(statement: Statement, indent: String): String = {
    statement match {
      case anImport: Import => generateImport(anImport, indent)
      case interface: Interface => generateInterface(interface, indent)
      case attribute: Attribute => generateAttribute(attribute, indent)
      case function: Function => generateFunction(function, indent)
      case const: Const => generateConst(const, indent)
    }
  }

  private def generateImport(anImport: Import, indent: String): String = {
    anImport match {
      case ModuleImport(module) => s"import * as ${StringUtils.lastPart(module, '/')} from ${quote(module)}"
      case ElementsImport(module, elements) => s"import {${elements.mkString(", ")}} from ${quote(module)}"
    }
  }

  private def generateInterface(interface: Interface, indent: String): String = {
    val properties = propertiesPrefix("export " -> interface.export)
    val firstLine = s"${indent}${properties}interface ${interface.name} {"
    val content = interface.attributes.map(generateStatement(_, inc(indent))).mkString("\n")
    val lastLine = s"${indent}}"
    firstLine + "\n" + content + "\n" + lastLine
  }

  private def generateAttribute(attribute: Attribute, indent: String): String = {
    s"${indent}${attribute.name}: ${generateType(attribute.typ)}"
  }

  protected def generateType(typ: Type): String = {
    typ match {
      case UnionType(left, right) =>
        s"${generateType(left)} | ${generateType(right)}"
      case ConcreteType(name, _, generics) =>
        if (generics.nonEmpty) {
          s"${StringUtils.lastPart(name)}<${generics.map(generateType).mkString(", ")}>"
        } else {
          StringUtils.lastPart(name)
        }
      case FunctionType(parameters, returnType) =>
        val parametersCode = s"${parameters.map(generateType).mkString(", ")}"
        val returnTypeCode = generateType(returnType)
        s"(${parametersCode}) => ${returnTypeCode}"
    }
  }

  private def propertiesPrefix(properties: (String, Boolean)*): String = {
    properties.collect {
      case (property, true) => property
    }.mkString(" ")
  }

  private def generateFunction(function: Function, indent: String): String = {
    val properties = propertiesPrefix("export " -> function.export)
    val parameters = function.parameters.map(generateParameter).mkString(", ")
    val returnType = generateType(function.returnType)
    val firstLine = s"${indent}${properties}function ${function.name}(${parameters}): ${returnType} {"
    val content = generateExpression(function.expression, inc(indent))
    val lastLine = s"${indent}}"
    firstLine + "\n" + content + "\n" + lastLine
  }

  protected def generateParameter(parameter: Parameter): String = {
    s"${parameter.name}: ${generateType(parameter.typ)}"
  }

  private def generateConst(const: Const, indent: String): String = {
    val properties = propertiesPrefix("export " -> const.export)
    val typ = generateType(const.typ)
    val expression = generateInlineExpression(const.body, indent)
    s"${properties}const ${const.name}: ${typ} = ${expression}"
  }
}
