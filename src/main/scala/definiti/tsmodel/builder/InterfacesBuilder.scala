package definiti.tsmodel.builder

import definiti.core.ast.{AttributeDefinition, DefinedType, Namespace}
import definiti.tsmodel.TsAST

trait InterfacesBuilder {
  self: ModelBuilder =>

  def buildInterfaces(namespace: Namespace): Seq[TsAST.TopLevelStatement] = {
    namespace.elements.collect {
      case definedType: DefinedType => buildInterface(definedType)
    }
  }

  private def buildInterface(definedType: DefinedType): TsAST.TopLevelStatement = {
    TsAST.Interface(
      name = definedType.name,
      attributes = definedType.attributes.map(buildAttribute),
      export = true
    )
  }

  private def buildAttribute(attributeDefinition: AttributeDefinition): TsAST.Attribute = {
    TsAST.Attribute(
      name = attributeDefinition.name,
      typ = buildTsType(attributeDefinition.typeReference)
    )
  }
}
