package definiti.tsmodel.helpers

import definiti.tsmodel.TsAST._

object ASTHelper {
  val noVerification = AttributeCall("verifications", "none")

  def verificationType(innerType: String): Type = {
    verificationType(Type(innerType))
  }
  def verificationType(innerType: Type): Type = {
    ConcreteType(
      name = "Verification",
      fullName = "definiti.native.verifications.Verification",
      generics = Seq(innerType)
    )
  }
}
