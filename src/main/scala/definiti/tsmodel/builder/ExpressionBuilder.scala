package definiti.tsmodel.builder

import definiti.core.ast._
import definiti.tsmodel.TsAST

trait ExpressionBuilder {
  self: ModelBuilder =>

  def buildExpression(expression: Expression): TsAST.Expression = {
    expression match {
      case logical: LogicalExpression => buildLogicalExpression(logical)
      case calculator: CalculatorExpression => buildCalculatorExpression(calculator)
      case not: Not => buildNot(not)
      case booleanValue: BooleanValue => buildBooleanValue(booleanValue)
      case numberValue: NumberValue => buildNumberValue(numberValue)
      case quoteStringValue: QuotedStringValue => buildQuoteStringValue(quoteStringValue)
      case reference: Reference => buildReference(reference)
      case methodCall: MethodCall => buildMethodCall(methodCall)
      case attributeCall: AttributeCall => buildAttributeCall(attributeCall)
      case combinedExpression: CombinedExpression => buildCombinedExpression(combinedExpression)
      case condition: Condition => buildCondition(condition)
      case lambdaExpression: LambdaExpression => buildLambdaExpression(lambdaExpression)
      case functionCall: FunctionCall => buildFunctionCall(functionCall)
    }
  }

  private def buildLogicalExpression(logicalExpression: LogicalExpression): TsAST.Expression = {
    TsAST.BinaryExpression(
      logicalOperatorToString(logicalExpression.operator),
      buildExpression(logicalExpression.left),
      buildExpression(logicalExpression.right)
    )
  }

  private def logicalOperatorToString(logicalOperator: LogicalOperator.Value): String = {
    import LogicalOperator._
    logicalOperator match {
      case Or => "||"
      case And => "&&"
      case Equal => "==="
      case NotEqual => "!=="
      case Lower => "<"
      case Upper => ">"
      case LowerOrEqual => "<="
      case UpperOrEqual => ">="
    }
  }

  private def buildCalculatorExpression(calculatorExpression: CalculatorExpression): TsAST.Expression = {
    TsAST.BinaryExpression(
      calculatorOperatorToString(calculatorExpression.operator),
      buildExpression(calculatorExpression.left),
      buildExpression(calculatorExpression.right)
    )
  }

  private def calculatorOperatorToString(calculatorOperator: CalculatorOperator.Value): String = {
    import CalculatorOperator._
    calculatorOperator match {
      case Plus => "+"
      case Minus => "-"
      case Modulo => "%"
      case Time => "*"
      case Divide => "/"
    }
  }

  private def buildNot(not: Not): TsAST.Expression = {
    TsAST.Not(buildExpression(not.inner))
  }

  private def buildBooleanValue(booleanValue: BooleanValue): TsAST.Expression = {
    TsAST.BooleanValue(booleanValue.value)
  }

  private def buildNumberValue(numberValue: NumberValue): TsAST.Expression = {
    TsAST.NumberValue(numberValue.value)
  }

  private def buildQuoteStringValue(quotedStringValue: QuotedStringValue): TsAST.Expression = {
    TsAST.StringValue(quotedStringValue.value)
  }

  private def buildReference(reference: Reference): TsAST.Expression = {
    TsAST.Reference(reference.name)
  }

  private def buildMethodCall(methodCall: MethodCall): TsAST.Expression = {
    methodCall.expression.returnType match {
      case namedFunctionReference: NamedFunctionReference =>
        throw new RuntimeException(s"Unexpected named function reference: ${namedFunctionReference.readableString}")
      case lambdaReference: LambdaReference =>
        throw new RuntimeException(s"Unexpected lambda reference: ${lambdaReference.readableString}")
      case typeReference: TypeReference =>
        TsAST.MethodCall(
          expression = TsAST.Reference(s"${typeReference.typeName}Extension"),
          method = methodCall.method,
          parameters = buildExpression(methodCall.expression) +: methodCall.parameters.map(buildExpression),
          generics = methodCall.generics.map(buildTsType(_))
        )
    }
  }

  private def buildAttributeCall(attributeCall: AttributeCall): TsAST.Expression = {
    attributeCall.expression.returnType match {
      case namedFunctionReference: NamedFunctionReference =>
        throw new RuntimeException(s"Unexpected named function reference: ${namedFunctionReference.readableString}")
      case lambdaReference: LambdaReference =>
        throw new RuntimeException(s"Unexpected lambda reference: ${lambdaReference.readableString}")
      case typeReference: TypeReference =>
        library.types.get(typeReference.typeName) match {
          case Some(_: NativeClassDefinition) =>
            TsAST.MethodCall(
              expression = TsAST.Reference(s"${typeReference.typeName}Extension"),
              method = attributeCall.attribute,
              parameters = Seq(buildExpression(attributeCall.expression)),
              generics = Seq.empty
            )
          case _ =>
            TsAST.AttributeCall(
              expression = buildExpression(attributeCall.expression),
              attribute = attributeCall.attribute
            )
        }
    }
  }

  private def buildCombinedExpression(combinedExpression: CombinedExpression): TsAST.Expression = {
    val expressions = combinedExpression.parts.map(buildExpression)
    if (expressions.lengthCompare(1) == 0) {
      expressions.head
    } else {
      TsAST.Expressions(expressions)
    }
  }

  private def buildCondition(condition: Condition): TsAST.Expression = {
    TsAST.Condition(
      condition = buildExpression(condition.condition),
      onTrue = buildExpression(condition.onTrue),
      onFalse = condition.onFalse.map(buildExpression)
    )
  }

  private def buildLambdaExpression(lambdaExpression: LambdaExpression): TsAST.Expression = {
    TsAST.ArrowFunction(
      parameters = lambdaExpression.parameterList.map(buildParameter),
      expression = buildExpression(lambdaExpression.expression)
    )
  }

  private def buildFunctionCall(functionCall: FunctionCall): TsAST.Expression = {
    TsAST.FunctionCall(
      name = functionCall.name,
      parameters = functionCall.parameters.map(buildExpression),
      generics = functionCall.generics.map(buildTsType(_))
    )
  }
}
