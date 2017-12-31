package definiti.tsmodel.builder

import definiti.core.ast._
import definiti.tsmodel.TsAST
import definiti.tsmodel.utils.ListUtils

trait VerificationBuilder {
  self: ModelBuilder =>

  def buildVerifications(namespace: Namespace): Seq[TsAST.TopLevelStatement] = {
    namespace.elements.collect {
      case verification: Verification => buildVerification(verification)
      case definedType: DefinedType => buildDefinedTypeVerification(definedType, namespace)
    }
  }

  private def buildVerification(verification: Verification): TsAST.TopLevelStatement = {
    TsAST.Const(
      name = s"${verification.name}Verification",
      typ = verificationType(buildAbstractTsType(verification.function.parameters.head.typeReference, genericAsAny = true)),
      body = TsAST.MethodCall(
        "verifications",
        "simple",
        Seq(
          TsAST.StringValue(verification.message),
          TsAST.ArrowFunction(
            parameters = verification.function.parameters.map(buildParameter),
            expression = buildExpression(verification.function.body)
          )
        )
      ),
      export = true
    )
  }

  private def buildDefinedTypeVerification(definedType: DefinedType, namespace: Namespace): TsAST.TopLevelStatement = {
    val typName = if (namespace.fullName.nonEmpty) namespace.fullName + "." + definedType.name else definedType.name
    TsAST.Const(
      name = s"${definedType.name}Verification",
      typ = verificationType(TsAST.Type(typName)),
      body = buildDefinedTypeVerificationBody(definedType),
      export = true
    )
  }

  private def buildDefinedTypeVerificationBody(definedType: DefinedType): TsAST.Expression = {
    val attributeVerifications = definedType.attributes.flatMap(buildAttributeVerification).map(lazyExpression)
    val inheritedVerifications = definedType.inherited.map(lazyVerification)
    val innerVerifications = buildInnerVerifications(definedType)
    val allVerifications = attributeVerifications ++ inheritedVerifications ++ innerVerifications
    buildVerificationWrapperExpression(allVerifications)
  }

  private def buildAttributeVerification(attributeDefinition: AttributeDefinition): Option[TsAST.Expression] = {
    val typeVerifications = buildTypeVerificationCall(attributeDefinition.typeReference).toSeq
    val attributeVerifications = attributeDefinition.verifications.map(verificationCall)
    val allVerifications = (typeVerifications ++ attributeVerifications)
      .map(TsAST.MethodCall(_, "from", Seq(TsAST.StringValue(attributeDefinition.name)), Seq.empty))
      .map(lazyExpression)
    if (allVerifications.nonEmpty) {
      Some(buildVerificationWrapperExpression(allVerifications))
    } else {
      None
    }
  }

  private def buildTypeVerificationCall(typeReference: TypeReference): Option[TsAST.Expression] = {
    library.types.get(typeReference.typeName) match {
      case Some(native: NativeClassDefinition) =>
        if (native.name == "List" || native.name == "Option") {
          typeReference.genericTypes.headOption
            .flatMap(buildTypeVerificationCall).map(innerVerification => TsAST.MethodCall(
              "verifications", native.name.toLowerCase, Seq(TsAST.ArrowFunction(Seq.empty, innerVerification))
            ))
        } else {
          None
        }

      case Some(_: DefinedType) =>
        Some(TsAST.Reference(s"${typeReference.typeName}Verification"))

      case Some(aliasType: AliasType) =>
        Some {
          val alias = aliasType.alias.copy(
            genericTypes = ListUtils.replaceOrdered(aliasType.alias.genericTypes, typeReference.genericTypes)
          )
          buildTypeVerificationCall(alias) match {
            case Some(innerVerification) =>
              TsAST.MethodCall(
                expression = innerVerification,
                method = "andThen",
                parameters = Seq(TsAST.Reference(s"${typeReference.typeName}Verification")),
                generics = Seq.empty
              )
            case None =>
              TsAST.Reference(s"${typeReference.typeName}Verification")
          }
        }

      case _ => None
    }
  }

  private def verificationCall(verificationReference: VerificationReference): TsAST.Expression = {
    verificationReference.message match {
      case Some(message) =>
        TsAST.MethodCall(
          TsAST.Reference(s"${verificationReference.verificationName}Verification"),
          "withMessage",
          Seq(TsAST.StringValue(message)),
          Seq.empty
        )
      case None =>
        TsAST.Reference(s"${verificationReference.verificationName}Verification")
    }
  }

  private def lazyExpression(expression: TsAST.Expression): TsAST.Expression = {
    TsAST.ArrowFunction(Seq.empty, expression)
  }

  private def lazyVerification(verificationReference: VerificationReference): TsAST.Expression = {
    lazyExpression(verificationCall(verificationReference))
  }

  private def buildVerificationWrapperExpression(allVerifications: Seq[TsAST.Expression]): TsAST.Expression = {
    if (allVerifications.isEmpty) {
      TsAST.AttributeCall("verifications", "none")
    } else if (allVerifications.lengthCompare(1) == 0) {
      TsAST.MethodCall("verifications", "lazy", Seq(allVerifications.head))
    } else {
      TsAST.MethodCall("verifications", "all", allVerifications)
    }
  }

  private def buildInnerVerifications(definedType: DefinedType): Seq[TsAST.Expression] = {
    definedType.verifications
      .map { innerVerification =>
        TsAST.MethodCall(
          expression = "verifications",
          method = "simple",
          parameters = Seq(
            TsAST.StringValue(innerVerification.message),
            TsAST.ArrowFunction(
              parameters = innerVerification.function.parameters.map(buildParameter),
              expression = buildExpression(innerVerification.function.body)
            )
          )
        )
      }
      .map(lazyExpression)
  }

  private def verificationType(innerType: TsAST.Type): TsAST.Type = {
    TsAST.Type("Verification", "definiti.native.verifications.Verification", Seq(innerType))
  }
}
