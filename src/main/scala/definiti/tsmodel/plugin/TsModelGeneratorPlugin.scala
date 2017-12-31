package definiti.tsmodel.plugin

import java.nio.file.Path

import definiti.core.GeneratorPlugin
import definiti.core.ast.{Library, Root}
import definiti.tsmodel.FileConfiguration
import definiti.tsmodel.builder.ModelBuilder
import definiti.tsmodel.generator.ProjectGenerator
import definiti.tsmodel.utils.Resource

class TsModelGeneratorPlugin extends GeneratorPlugin {
  val config = new FileConfiguration()
  private val nativeSourceDirectory: Resource = Resource("native")
  private val destinationDirectory: Path = config.destination.resolve("definiti").resolve("native")

  override def name: String = "ts-model-generator"

  override def generate(root: Root, library: Library): Map[Path, String] = {
    nativeSources ++ generatedSources(root, library)
  }

  def nativeSources: Map[Path, String] = {
    nativeSourceDirectory
      .children
      .filterNot(_.isDirectory)
      .map(file => destinationDirectory.resolve(file.name) -> file.content)
      .toMap
  }

  def generatedSources(root: Root, library: Library): Map[Path, String] = {
    val scalaRoot = new ModelBuilder(config, library).build(root)
    val scalaFiles = new ProjectGenerator(config).generateProject(scalaRoot)
    scalaFiles
      .map(tsFile => tsFile.path -> tsFile.content)
      .toMap
  }
}
