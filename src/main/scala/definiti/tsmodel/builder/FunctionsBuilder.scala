package definiti.tsmodel.builder

import definiti.core.ast.{NamedFunction, Namespace, ParameterDefinition}
import definiti.tsmodel.TsAST

trait FunctionsBuilder {
  self: ModelBuilder =>

  def buildFunctions(namespace: Namespace): Seq[TsAST.TopLevelStatement] = {
    namespace.elements.collect {
      case namedFunction: NamedFunction => buildFunction(namedFunction)
    }
  }

  def buildFunction(namedFunction: NamedFunction): TsAST.TopLevelStatement = {
    TsAST.Function(
      name = namedFunction.name,
      parameters = namedFunction.parameters.map(buildParameter),
      expression = buildExpression(namedFunction.body),
      returnType = buildTsType(namedFunction.returnType),
      export = true
    )
  }

  def buildParameter(parameterDefinition: ParameterDefinition): TsAST.Parameter = {
    TsAST.Parameter(
      name = parameterDefinition.name,
      typ = buildAbstractTsType(parameterDefinition.typeReference)
    )
  }
}
