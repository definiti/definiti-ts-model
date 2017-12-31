package definiti.tsmodel.generator

import definiti.tsmodel.Configuration
import definiti.tsmodel.TsAST.{Root, TsFile}

class ProjectGenerator(config: Configuration) {
  def generateProject(root: Root): Seq[TsFile] = {
    root.modules
      .map { module =>
        val dirname = module.name.replaceAllLiterally(".", "/")
        val filename = "index.ts"
        val path = config.destination.resolve(dirname).resolve(filename)
        TsFile(path, CodeGenerator.generate(module))
      }
  }
}
