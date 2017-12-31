package definiti.tsmodel.helpers

import java.nio.file.{Path, Paths}

import definiti.tsmodel.Configuration
import definiti.core.{Configuration => CoreConfiguration, _}

case class ConfigurationMock(
  destination: Path = Paths.get("")
) extends Configuration

case class CoreConfigurationMock(
  source: Path = Paths.get(""),
  apiSource: Path = Paths.get(""),
  parsers: Seq[ParserPlugin] = Seq.empty,
  validators: Seq[ValidatorPlugin] = Seq.empty,
  generators: Seq[GeneratorPlugin] = Seq.empty,
  contexts: Seq[ContextPlugin[_]] = Seq.empty
) extends CoreConfiguration