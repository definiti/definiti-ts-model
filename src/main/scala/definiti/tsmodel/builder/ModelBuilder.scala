package definiti.tsmodel.builder

import definiti.core.ast.{Library, Namespace, Root}
import definiti.tsmodel.{Configuration, TsAST}

class ModelBuilder(config: Configuration, val library: Library)
  extends ExpressionBuilder
    with FunctionsBuilder
    with ImportBuilder
    with InterfacesBuilder
    with TypeBuilder
    with VerificationBuilder {
  def build(root: Root): TsAST.Root = {
    val rootNamespace = Namespace("", "", root.elements)
    val metaModules = extractModules(rootNamespace).filter(_._2.elements.nonEmpty)
    val modules = metaModules.map(meta => buildModule(meta._1, meta._2))
    TsAST.Root(modules)
  }

  private def extractModules(namespace: Namespace): Seq[(String, Namespace)] = {
    val children = namespace.elements.collect {
      case child: Namespace => extractModules(child)
    }.flatten
    val cleanedNamespace = namespace.copy(elements = namespace.elements.filterNot(_.isInstanceOf[Namespace]))
    val current = namespace.fullName -> cleanedNamespace
    current +: children
  }

  private def buildModule(name: String, namespace: Namespace): TsAST.Module = {
    val moduleWithoutImports = TsAST.Module(
      name = name,
      statements = buildInterfaces(namespace) ++ buildFunctions(namespace) ++ buildVerifications(namespace)
    )
    moduleWithoutImports.copy(statements = buildImports(moduleWithoutImports) ++ moduleWithoutImports.statements)
  }
}
