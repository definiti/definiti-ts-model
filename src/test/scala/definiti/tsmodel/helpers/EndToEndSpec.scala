package definiti.tsmodel.helpers

import java.nio.file.Paths

import definiti.core.{Configuration => CoreConfiguration, _}
import definiti.tsmodel.builder.ModelBuilder
import definiti.tsmodel.{Configuration, TsAST}
import org.scalatest.{FlatSpec, Matchers}

trait EndToEndSpec extends FlatSpec with Matchers with ASTMatcher {
  def processDirectory(sample: String, configuration: Configuration = ConfigurationMock()): Validated[TsAST.Root] = {
    process(configurationDirectory(sample), configuration)
  }

  def configurationDirectory(sample: String): CoreConfiguration = {
    CoreConfigurationMock(
      source = Paths.get(s"src/test/resources/samples/${sample.replaceAll("\\.", "/")}"),
      apiSource = Paths.get(s"src/main/resources/api"),
      contexts = Seq()
    )
  }

  def processFile(sample: String, configuration: Configuration = ConfigurationMock()): Validated[TsAST.Root] = {
    process(configurationFile(sample), configuration)
  }

  def configurationFile(sample: String): CoreConfiguration = {
    CoreConfigurationMock(
      source = Paths.get(s"src/test/resources/samples/${sample.replaceAll("\\.", "/")}.def"),
      apiSource = Paths.get(s"src/main/resources/api"),
      contexts = Seq()
    )
  }

  private def process(coreConfiguration: CoreConfiguration, configuration: Configuration): Validated[TsAST.Root] = {
    val project = new Project(coreConfiguration)
    project.generateStructureWithLibrary()
      .map { case (ast, library) =>
        new ModelBuilder(configuration, library).build(ast)
      }
  }
}