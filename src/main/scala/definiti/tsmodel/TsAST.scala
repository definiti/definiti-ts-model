package definiti.tsmodel

import java.nio.file.Path

object TsAST {

  sealed trait Statement

  sealed trait TopLevelStatement extends Statement

  case class Root(modules: Seq[Module])

  object Root {
    def apply(modules: Module*)(implicit dummyImplicit: DummyImplicit): Root = {
      new Root(modules)
    }
  }

  case class Module(name: String, statements: Seq[TopLevelStatement])

  object Module {
    def apply(name: String, statements: TopLevelStatement*)(implicit dummyImplicit: DummyImplicit): Module = {
      new Module(name, statements)
    }
  }

  sealed trait Import extends TopLevelStatement

  case class ModuleImport(module: String) extends Import

  case class ElementsImport(module: String, elements: Seq[String]) extends Import

  object Import {
    def apply(module: String): Import = ModuleImport(module)

    def apply(module: String, elements: Seq[String]): Import = ElementsImport(module, elements)

    def apply(module: String, element: String): Import = ElementsImport(module, Seq(element))
  }

  case class Function(name: String, parameters: Seq[Parameter], returnType: Type, expression: Expression, export: Boolean) extends TopLevelStatement

  case class Parameter(name: String, typ: Type)

  sealed trait Type

  object Type {
    def apply(left: Type, right: Type): Type = UnionType(left, right)

    def apply(name: String, fullName: String, generics: Seq[Type]): Type = ConcreteType(name, fullName, generics)

    def apply(name: String, fullName: String, generics: Type*)(implicit dummyImplicit: DummyImplicit): Type = ConcreteType(name, fullName, generics)

    def apply(name: String, generics: Seq[Type]): Type = ConcreteType(name, name, generics)

    def apply(name: String, generics: Type*)(implicit dummyImplicit: DummyImplicit): Type = ConcreteType(name, name, generics)
  }

  case class UnionType(left: Type, right: Type) extends Type

  case class ConcreteType(name: String, fullName: String, generics: Seq[Type] = Seq.empty) extends Type

  case class FunctionType(parameters: Seq[Type], returnType: Type) extends Type

  case class Interface(name: String, attributes: Seq[Attribute], export: Boolean = false) extends TopLevelStatement

  case class Attribute(name: String, typ: Type) extends Statement

  case class Const(name: String, typ: Type, body: Expression, export: Boolean) extends TopLevelStatement

  sealed trait Expression

  case class BinaryExpression(operator: String, left: Expression, right: Expression) extends Expression

  case class Not(expression: Expression) extends Expression

  case class BooleanValue(value: Boolean) extends Expression

  case class NumberValue(value: BigDecimal) extends Expression

  case class StringValue(value: String) extends Expression

  case class Reference(name: String) extends Expression

  case class MethodCall(expression: Expression, method: String, parameters: Seq[Expression], generics: Seq[Type]) extends Expression

  object MethodCall {
    def apply(expression: String, method: String, parameters: Seq[Expression], generics: Seq[Type] = Seq.empty): MethodCall = {
      new MethodCall(Reference(expression), method, parameters, generics)
    }
  }

  case class AttributeCall(expression: Expression, attribute: String) extends Expression

  object AttributeCall {
    def apply(expression: String, attribute: String): AttributeCall = {
      new AttributeCall(Reference(expression), attribute)
    }
  }

  case class Expressions(expressions: Seq[Expression]) extends Expression

  case class Condition(condition: Expression, onTrue: Expression, onFalse: Option[Expression]) extends Expression

  case class ArrowFunction(parameters: Seq[Parameter], expression: Expression) extends Expression

  case class FunctionCall(name: String, parameters: Seq[Expression], generics: Seq[Type]) extends Expression

  case class TsFile(path: Path, content: String)

}