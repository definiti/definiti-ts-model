package definiti.tsmodel.generator

import definiti.tsmodel.TsAST._
import definiti.tsmodel.utils.StringUtils

trait ExpressionGenerator {
  self: CodeGenerator =>

  def generateExpression(expression: Expression, indent: String): String = {
    s"${indent}${generateExpression(expression, indent, isLast = true)}"
  }

  def generateInlineExpression(expression: Expression, indent: String): String = {
    s"${indent}${generateExpression(expression, indent, isLast = false)}"
  }

  private def generateExpression(expression: Expression, indent: String, isLast: Boolean): String = {
    def line(expression: String): String = {
      if (isLast) {
        s"return ${expression}"
      } else {
        expression
      }
    }

    expression match {
      case binaryExpression: BinaryExpression =>
        line(generateBinaryExpression(binaryExpression, indent))
      case not: Not =>
        line(generateNot(not, indent))
      case booleanValue: BooleanValue =>
        line(generateBooleanValue(booleanValue, indent))
      case numberValue: NumberValue =>
        line(generateNumberValue(numberValue, indent))
      case stringValue: StringValue =>
        line(generateStringValue(stringValue, indent))
      case reference: Reference =>
        line(generateReference(reference, indent))
      case methodCall: MethodCall =>
        line(generateMethodCall(methodCall, indent))
      case attributeCall: AttributeCall =>
        line(generateAttributeCall(attributeCall, indent))
      case expressions: Expressions =>
        generateExpressions(expressions, indent, isLast)
      case condition: Condition =>
        generateCondition(condition, indent, isLast)
      case arrowFunction: ArrowFunction =>
        line(generateArrowFunction(arrowFunction, indent))
      case functionCall: FunctionCall =>
        line(generateFunctionCall(functionCall, indent))
    }
  }

  private def generateBinaryExpression(expression: BinaryExpression, indent: String): String = {
    val left = generateExpression(expression.left, indent, isLast = false)
    val right = generateExpression(expression.right, indent, isLast = false)
    s"(${left}) ${expression.operator} (${right})"
  }

  private def generateNot(not: Not, indent: String): String = {
    s"!(${generateExpression(not.expression, indent, isLast = false)})"
  }

  private def generateBooleanValue(booleanValue: BooleanValue, indent: String): String = {
    booleanValue.value.toString
  }

  private def generateNumberValue(numberValue: NumberValue, indent: String): String = {
    numberValue.value.toString
  }

  private def generateStringValue(stringValue: StringValue, indent: String): String = {
    quote(stringValue.value)
  }

  private def generateReference(reference: Reference, indent: String): String = {
    StringUtils.lastPart(reference.name)
  }

  private def generateMethodCall(methodCall: MethodCall, indent: String): String = {
    val expression = generateExpression(methodCall.expression, indent, isLast = false)
    val parameters = methodCall.parameters.map(generateExpression(_, indent, isLast = false)).mkString(", ")
    s"${expression}.${methodCall.method}(${parameters})"
  }

  private def generateAttributeCall(attributeCall: AttributeCall, indent: String): String = {
    val expression = generateExpression(attributeCall.expression, indent, isLast = false)
    s"${expression}.${attributeCall.attribute}"
  }

  private def generateExpressions(expressions: Expressions, indent: String, isLast: Boolean): String = {
    expressions.expressions.length match {
      case 0 => if (isLast) "return undefined" else "undefined"
      case 1 => generateExpression(expressions.expressions.head, indent, isLast)
      case n =>
        val firstExpressions = expressions.expressions.slice(0, n-1).map(generateExpression(_, indent, isLast = false))
        val lastExpression = generateExpression(expressions.expressions.last, indent, isLast)
        (firstExpressions :+ lastExpression).mkString(s"\n${indent}")
    }
  }

  private def generateCondition(condition: Condition, indent: String, isLast: Boolean): String = {
    val conditionExpression = generateExpression(condition.condition, inc(indent), isLast = false)
    val onTrueExpression = generateExpression(condition.onTrue, inc(indent), isLast)
    condition.onFalse match {
      case Some(onFalse) =>
        val onFalseExpression = generateExpression(onFalse, inc(indent), isLast)
        val ifLine = s"if (${conditionExpression}) {"
        val elseLine = s"${indent}} else {"
        val endLine = s"${indent}}"

        Seq(ifLine, inc(indent) + onTrueExpression, elseLine, inc(indent) + onFalseExpression, endLine).mkString("\n")
      case None =>
        val ifLine = s"if (${conditionExpression}) {"
        val endLine = s"${indent}}"

        Seq(ifLine, inc(indent) + onTrueExpression, endLine).mkString("\n")
    }
  }

  private def generateArrowFunction(arrowFunction: ArrowFunction, indent: String): String = {
    val inputs = arrowFunction.parameters.map(generateParameter).mkString(", ")
    val expression = generateExpression(arrowFunction.expression, inc(indent), isLast = false)
    s"(${inputs}) => ${expression}"
  }

  private def generateFunctionCall(functionCall: FunctionCall, indent: String): String = {
    val name = StringUtils.lastPart(functionCall.name)
    val parameters = functionCall.parameters.map(generateExpression(_, indent, isLast = false)).mkString(", ")
    val generics = generateGenerics(functionCall.generics)
    s"${name}${generics}(${parameters})"
  }

  private def generateGenerics(generics: Seq[Type]): String = {
    if (generics.nonEmpty) {
      s"<${generics.map(generateType).mkString(", ")}>"
    } else {
      ""
    }
  }
}
