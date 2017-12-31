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
}

object NominalSpec {
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
}