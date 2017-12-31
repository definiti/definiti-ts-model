package definiti.tsmodel.builder

import definiti.core.ast._
import definiti.tsmodel.TsAST

trait VerificationBuilder {
  self: ModelBuilder =>

  def buildVerifications(namespace: Namespace): Seq[TsAST.TopLevelStatement] = {
    namespace.elements.collect {
      case verification: Verification => buildVerification(verification)
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

  private def verificationType(innerType: TsAST.Type): TsAST.Type = {
    TsAST.Type("Verification", "definiti.native.verifications.Verification", Seq(innerType))
  }
}
