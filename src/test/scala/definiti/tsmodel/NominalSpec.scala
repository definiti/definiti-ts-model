package definiti.tsmodel

import definiti.tsmodel.TsAST._
import definiti.tsmodel.helpers.EndToEndSpec

class NominalSpec extends EndToEndSpec {
  import NominalSpec._

  "The generator" should "generate a valid scala AST for a valid named function" in {
    val output = processFile("nominal.namedFunction")
    output should beValidRoot(namedFunction)
  }

  it should "generate nothing from an extended context" in {
    val output = processFile("nominal.extendedContext")
    output should beValidRoot(extendedContext)
  }

  it should "generate a valid scala AST for a valid verification" in {
    val output = processFile("nominal.verification")
    output should beValidRoot(verification)
  }

  it should "generate a valid scala AST for a valid defined type" in {
    val output = processFile("nominal.definedType")
    output should beValidRoot(definedType)
  }
}

object NominalSpec {
  import definiti.tsmodel.helpers.ASTHelper._

  val namedFunction: Root = Root(
    Module(
      name = "",
      Function(
        name = "alwaysFalse",
        parameters = Seq.empty,
        returnType = Type("boolean"),
        expression = BooleanValue(false),
        export = true
      )
    )
  )

  val extendedContext: Root = Root()

  val verification: Root = Root(
    Module(
      name = "",
      ModuleImport("../definiti/native/verifications"),
      ElementsImport("../definiti/native/verifications", Seq("Verification")),
      Const(
        name = "AlwaysTrueVerification",
        typ = verificationType("string"),
        body = MethodCall(
          expression = "verifications",
          method = "simple",
          parameters = Seq(
            StringValue("Never fail"),
            ArrowFunction(Seq(Parameter("x", Type("string"))), BooleanValue(true))
          )
        ),
        export = true
      )
    )
  )

  val definedType: Root = Root(
    Module(
      name = "",
      ModuleImport("../definiti/native/verifications"),
      ElementsImport("../definiti/native/verifications", Seq("Verification")),
      Interface(
        name = "MyType",
        attributes = Seq(Attribute("myAttribute", Type("string"))),
        export = true
      ),
      Const(
        name = "MyTypeVerification",
        typ = verificationType("MyType"),
        body = noVerification,
        export = true
      )
    )
  )

}