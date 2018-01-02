package definiti.tsmodel.helpers

import definiti.core.{Error, Invalid, ValidValue, Validated}
import definiti.tsmodel.TsAST.Root
import definiti.tsmodel.generator.CodeGenerator
import org.scalatest.matchers.{MatchResult, Matcher}

trait ASTMatcher {

  class ValidatedRootMatcher(expected: Validated[Root]) extends Matcher[Validated[Root]] {
    def apply(left: Validated[Root]): MatchResult = {
      (expected, left) match {
        case (ValidValue(expectedValue), ValidValue(gotValue)) =>
          if (expectedValue == gotValue) {
            success
          } else {
            failed(expected, left)
          }
        case (ValidValue(_), Invalid(_)) => failed(expected, left)
        case (Invalid(_), ValidValue(_)) => failed(expected, left)
        case (Invalid(expectedErrors), Invalid(gotErrors)) =>
          val missingErrors = missingExpectedErrors(expectedErrors, gotErrors)
          val additionalErrors = additionalGotErrors(expectedErrors, gotErrors)
          if (missingErrors.nonEmpty || additionalErrors.nonEmpty) {
            failed(expected, left)
          } else {
            success
          }
      }
    }

    private def failed(expected: Validated[Root], got: Validated[Root]) = MatchResult(matches = false, s"${prettyPrint(expected)} did not equal ${prettyPrint(got)}", "")

    private def prettyPrint(validation: Validated[Root]): String = validation match {
      case ValidValue(root) => s"ValidValue(${printRoot(root)})"
      case invalid: Invalid => invalid.prettyPrint
    }

    private def missingExpectedErrors(expectedErrors: Seq[Error], gotErrors: Seq[Error]): Seq[Error] = {
      expectedErrors.filter(expectedError => !gotErrors.contains(expectedError))
    }

    private def additionalGotErrors(expectedErrors: Seq[Error], gotErrors: Seq[Error]): Seq[Error] = {
      gotErrors.filter(gotError => !expectedErrors.contains(gotError))
    }

    private val success = MatchResult(matches = true, "", "")

    private def printRoot(root: Root): String = {
      s"""__root__ {
         |${root.modules.map(CodeGenerator.generate)}
         |}
         |""".stripMargin
    }
  }

  def beValidRoot(expected: Root) = new ValidatedRootMatcher(ValidValue(expected))
}

object ASTMatcher extends ASTMatcher
