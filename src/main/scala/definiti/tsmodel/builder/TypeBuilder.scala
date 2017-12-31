package definiti.tsmodel.builder

import definiti.core.ast._
import definiti.tsmodel.TsAST
import definiti.tsmodel.utils.{ListUtils, StringUtils}

trait TypeBuilder {
  self: ModelBuilder =>

  def buildAbstractTsType(abstractTypeReference: AbstractTypeReference, genericAsAny: Boolean = false): TsAST.Type = {
    abstractTypeReference match {
      case typeReference: TypeReference => buildType(typeReference, Seq.empty, genericAsAny)
      case lambdaReference: LambdaReference => buildLambdaType(lambdaReference, genericAsAny)
      case namedFunctionReference: NamedFunctionReference => buildFunctionType(namedFunctionReference, genericAsAny)
    }
  }

  def buildTsType(typeReference: TypeReference, genericAsAny: Boolean = false): TsAST.Type = {
    buildType(typeReference, Seq.empty, genericAsAny)
  }

  private def buildType(typeReference: TypeReference, outerTypes: Seq[TypeReference], genericAsAny: Boolean): TsAST.Type = {
    library.types.get(typeReference.typeName) match {
      case Some(_: NativeClassDefinition) =>
        buildFromNativeType(typeReference, outerTypes, genericAsAny)
      case Some(aliasType: AliasType) =>
        buildType(aliasType.alias, ListUtils.replaceOrdered(aliasType.alias.genericTypes, typeReference.genericTypes), genericAsAny)
      case None =>
        TsAST.Type("any")
      case _ =>
        TsAST.Type(
          name = StringUtils.lastPart(typeReference.typeName),
          fullName = typeReference.typeName,
          generics = ListUtils.replaceOrdered(typeReference.genericTypes, outerTypes).map(buildType(_, Seq.empty, genericAsAny))
        )
    }
  }

  private def buildFromNativeType(typeReference: TypeReference, outerTypes: Seq[TypeReference], genericsAsAny: Boolean): TsAST.Type = {
    typeReference.typeName match {
      case "Any" => TsAST.Type("any")
      case "Boolean" => TsAST.Type("boolean")
      case "Date" => TsAST.Type("Date")
      case "List" =>
        TsAST.Type(
          name = "Array",
          generics = ListUtils.replaceOrdered(typeReference.genericTypes, outerTypes).map(buildTsType(_, genericsAsAny))
        )
      case "Number" => TsAST.Type("number")
      case "Option" =>
        val genericTypes = ListUtils.replaceOrdered(typeReference.genericTypes, outerTypes)
        genericTypes.headOption match {
          case Some(innerTypeReference) =>
            TsAST.Type(
              buildType(innerTypeReference, Seq.empty, genericsAsAny),
              TsAST.Type("undefined")
            )
          case None =>
            throw new RuntimeException(s"An option does not have an internal generic type (type: ${typeReference.readableString})")
        }
      case "String" => TsAST.Type("string")
      case "Unit" => TsAST.Type("void")
    }
  }

  private def buildLambdaType(lambdaReference: LambdaReference, genericAsAny: Boolean): TsAST.Type = {
    TsAST.FunctionType(
      parameters = lambdaReference.inputTypes.map(buildTsType(_, genericAsAny)),
      returnType = buildTsType(lambdaReference.outputType, genericAsAny)
    )
  }

  private def buildFunctionType(namedFunctionReference: NamedFunctionReference, genericAsAny: Boolean): TsAST.Type = {
    library.namedFunctions.get(namedFunctionReference.functionName) match {
      case Some(namedFunction) =>
        TsAST.FunctionType(
          parameters = namedFunction.parameters.map(parameter => buildAbstractTsType(parameter.typeReference, genericAsAny)),
          returnType = buildTsType(namedFunction.returnType, genericAsAny)
        )
      case None =>
        sys.error(s"Named function ${namedFunctionReference.functionName} does not exist")
    }
  }

  private val nativeTypes = Seq("any", "boolean", "Date", "Array", "number", "undefined", "string", "void")
  def isNative(typ: TsAST.ConcreteType): Boolean = {
    nativeTypes.contains(typ.name)
  }
}
