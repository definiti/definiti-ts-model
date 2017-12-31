package definiti.tsmodel.plugin

import java.nio.file.Path

import definiti.core.GeneratorPlugin
import definiti.core.ast.{Library, Root}

class TsModelGeneratorPlugin extends GeneratorPlugin {
  override def name: String = "ts-model-generator"

  override def generate(root: Root, library: Library): Map[Path, String] = Map.empty
}
