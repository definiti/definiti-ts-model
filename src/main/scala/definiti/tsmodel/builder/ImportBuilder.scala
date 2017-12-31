package definiti.tsmodel.builder

import definiti.tsmodel.TsAST
import definiti.tsmodel.utils.StringUtils

trait ImportBuilder {
  self: ModelBuilder =>

  def buildImports(module: TsAST.Module): Seq[TsAST.TopLevelStatement] = {
    groupImports {
      commonImports(module) ++
        extractImportsFromTypes(module)
          .distinct
          .filterNot(StringUtils.excludeLastPart(_) == module.name)
          .map(createImport(module, _))
    }
  }

  private def commonImports(module: TsAST.Module): Seq[TsAST.Import] = {
    val extensions = Seq("Boolean", "Date", "List", "Number", "Option", "String").map(typeName => s"${typeName}Extension")
    val validation = Seq("verifications")
    val moduleImports = extensions ++ validation
    moduleImports
      .filter(isModuleUsed(_, module))
      .map(moduleName => s"definiti.native.${moduleName}")
      .map(createModuleImport(module, _))
  }

  private def isModuleUsed(importedModuleName: String, module: TsAST.Module): Boolean = {
    module.statements.exists {
      case function: TsAST.Function => isModuleUsedInExpression(importedModuleName, function.expression)
      case const: TsAST.Const => isModuleUsedInExpression(importedModuleName, const.body)
      case _ => false
    }
  }

  private def isModuleUsedInExpression(moduleName: String, expression: TsAST.Expression): Boolean = {
    expression match {
      case TsAST.BinaryExpression(_, left, right) =>
        isModuleUsedInExpression(moduleName, left) || isModuleUsedInExpression(moduleName, right)
      case TsAST.Not(inner) =>
        isModuleUsedInExpression(moduleName, inner)
      case TsAST.MethodCall(inner, _, parameters, generics) =>
        isModuleUsedInExpression(moduleName, inner) || parameters.exists(isModuleUsedInExpression(moduleName, _))
      case TsAST.AttributeCall(inner, _) =>
        isModuleUsedInExpression(moduleName, inner)
      case TsAST.Expressions(expressions) =>
        expressions.exists(isModuleUsedInExpression(moduleName, _))
      case TsAST.Condition(condition, onTrue, onFalse) =>
        isModuleUsedInExpression(moduleName, condition) || isModuleUsedInExpression(moduleName, onTrue) || onFalse.exists(isModuleUsedInExpression(moduleName, _))
      case TsAST.ArrowFunction(_, inner) =>
        isModuleUsedInExpression(moduleName, inner)
      case TsAST.FunctionCall(_, parameters, _) =>
        parameters.exists(isModuleUsedInExpression(moduleName, _))
      case TsAST.Reference(name) if name == moduleName => true
      case _ => false
    }
  }

  private def extractImportsFromTypes(module: TsAST.Module): Seq[String] = {
    module.statements.collect {
      case interface: TsAST.Interface => extractImportsFromInterface(interface)
      case function: TsAST.Function => extractImportsFromFunction(function)
      case const: TsAST.Const => extractImportsFromConst(const)
    }.flatten
  }

  private def extractImportsFromInterface(interface: TsAST.Interface): Seq[String] = {
    interface.attributes.flatMap(attribute => extractImportsFromType(attribute.typ))
  }

  private def extractImportsFromFunction(function: TsAST.Function): Seq[String] = {
    val returnImports = extractImportsFromType(function.returnType)
    val parameterImports = function.parameters.flatMap(parameter => extractImportsFromType(parameter.typ))
    val expressionImports = extractImportsFromExpression(function.expression)
    returnImports ++ parameterImports ++ expressionImports
  }

  private def extractImportsFromConst(const: TsAST.Const): Seq[String] = {
    val returnImports = extractImportsFromType(const.typ)
    val expressionImports = extractImportsFromExpression(const.body)
    returnImports ++ expressionImports
  }

  private def extractImportsFromType(typ: TsAST.Type): Seq[String] = {
    typ match {
      case TsAST.UnionType(left, right) => extractImportsFromType(left) ++ extractImportsFromType(right)
      case concreteType: TsAST.ConcreteType =>
        if (isNative(concreteType)) {
          concreteType.generics.flatMap(extractImportsFromType)
        } else {
          concreteType.fullName +: concreteType.generics.flatMap(extractImportsFromType)
        }
      case _ => Seq.empty
    }
  }

  private def extractImportsFromExpression(expression: TsAST.Expression): Seq[String] = {
    expression match {
      case TsAST.BinaryExpression(_, left, right) =>
        extractImportsFromExpression(left) ++ extractImportsFromExpression(right)
      case TsAST.Not(inner) =>
        extractImportsFromExpression(inner)
      case TsAST.MethodCall(inner, _, parameters, generics) =>
        extractImportsFromExpression(inner) ++ parameters.flatMap(extractImportsFromExpression) ++ generics.flatMap(extractImportsFromType)
      case TsAST.AttributeCall(inner, _) =>
        extractImportsFromExpression(inner)
      case TsAST.Expressions(expressions) =>
        expressions.flatMap(extractImportsFromExpression)
      case TsAST.Condition(condition, onTrue, onFalse) =>
        extractImportsFromExpression(condition) ++ extractImportsFromExpression(onTrue) ++ onFalse.toSeq.flatMap(extractImportsFromExpression)
      case TsAST.ArrowFunction(parameters, inner) =>
        parameters.flatMap(parameter => extractImportsFromType(parameter.typ)) ++ extractImportsFromExpression(inner)
      case TsAST.FunctionCall(name, parameters, generics) =>
        name +: (parameters.flatMap(extractImportsFromExpression) ++ generics.flatMap(extractImportsFromType))
      case TsAST.Reference(name) if name.contains(".") =>
        Seq(name)
      case _ =>
        Seq.empty
    }
  }

  private def createModuleImport(module: TsAST.Module, importName: String): TsAST.Import = {
    TsAST.Import(relativePath(module.name, importName))
  }

  private def createImport(module: TsAST.Module, importName: String): TsAST.Import = {
    TsAST.Import(relativePath(module.name, StringUtils.excludeLastPart(importName)), StringUtils.lastPart(importName))
  }

  private def relativePath(currentModule: String, targetModule: String): String = {
    val currentModuleParts = currentModule.split("\\.")
    val targetModuleParts = targetModule.split("\\.")
    val partsIgnored = currentModuleParts.zip(targetModuleParts).takeWhile { case (p1, p2) => p1 == p2 }.length
    // We do not know if the target is at the top of the hierarchy so we use relative path to ensure imports
    val goUp = Range(0, currentModuleParts.length - partsIgnored).map(_ => "..").mkString("/")
    val goDown = targetModuleParts.drop(partsIgnored).mkString("/")
    if (goUp.isEmpty) {
      "./" + goDown
    } else {
      goUp + "/" + goDown
    }
  }

  private def groupImports(imports: Seq[TsAST.Import]): Seq[TsAST.Import] = {
    val moduleImports = imports.collect { case moduleImport: TsAST.ModuleImport => moduleImport }
    val elementsImports = imports.collect { case elementsImport: TsAST.ElementsImport => elementsImport }
    val groupedElementImports =
      elementsImports
        .groupBy(_.module)
        .map { case (module, importsOfModule) => TsAST.Import(module, importsOfModule.flatMap(_.elements)) }

    moduleImports ++ groupedElementImports
  }
}
