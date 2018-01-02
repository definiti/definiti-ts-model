package definiti.tsmodel

import definiti.tsmodel.TsAST._
import definiti.tsmodel.helpers.EndToEndSpec

class DeepTypesSpec extends EndToEndSpec {
  import DeepTypesSpec._

  "The generator" should "consider the validation of the alias type" in {
    val output = processFile("deeptypes.attributeAsAliasType")
    output should beValidRoot(attributeAsAliasType)
  }
}

object DeepTypesSpec {
  import definiti.tsmodel.helpers.ASTHelper._

  val attributeAsAliasType: Root = Root(
    Module(
      name = "my",
      Import("../definiti/native/StringExtension"),
      Import("../definiti/native/verifications"),
      Import("../definiti/native/verifications", "Verification"),
      Interface(
        name = "MyType",
        attributes = Seq(Attribute("attribute", Type("string"))),
        export = true
      ),
      Const(
        name = "IsRequiredVerification",
        typ = verificationType("string"),
        body = MethodCall(
          expression = "verifications",
          method = "simple",
          parameters = Seq(
            StringValue("This string is required"),
            ArrowFunction(
              Seq(Parameter("string", Type("string"))),
              MethodCall("StringExtension", "nonEmpty", Seq(Reference("string")))
            )
          )
        ),
        export = true
      ),
      Const(
        name = "MyTypeVerification",
        typ = verificationType("my.MyType"),
        body = lazyVerification(
          lazyVerification(
            MethodCall("my.RequiredStringVerification", "from", Seq(StringValue("attribute")))
          )
        ),
        export = true
      ),
      Const(
        name = "RequiredStringVerification",
        typ = verificationType("string"),
        body = MethodCall(
          expression = "verifications",
          method = "lazy",
          parameters = Seq(
            ArrowFunction(Seq.empty, Reference("my.IsRequiredVerification"))
          )
        ),
        export = true
      )
    )
  )
}
