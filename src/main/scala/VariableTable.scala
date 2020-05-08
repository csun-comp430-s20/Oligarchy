class VariableTable {

  private var variables: Map[String, VariableEntry] = _
  private var nextIndex = 0

  @throws[CodeGeneratorException]
  def addEntry(formalParam: VarDeclaration): VariableEntry = {
    addEntry(formalParam.varName, formalParam.types)
  }

  @throws[CodeGeneratorException]
  def addEntry(variable: String, types: Types): VariableEntry = if (variables contains variable) { // should be caught by typechec// r
    throw new CodeGeneratorException("Variable already in scope: " + variable)
  }
  else {
    val entry = new VariableEntry(variable, types, {
      nextIndex += 1;
      nextIndex - 1
    })
    variables += ((variable -> entry))
    entry
  }

  @throws[CodeGeneratorException]
  def getEntryFor(variable: String): VariableEntry = {
    if (variables.contains(variable)) variables(variable)
    else throw new CodeGeneratorException("no such variable declared: " + variable)
    // getEntryFor
  }

  def hasEntryFor(variable: String): Boolean = {
    variables.contains(variable)
  }// hasEntryFor

  @throws[CodeGeneratorException]
  def withFormalParam(thisType: ClassTypes, paramType: ClassTypes, param: String): VariableTable = {
    val formalParams = List(VarDeclaration(paramType,param))
    withFormalParams(thisType, formalParams)
  }// withFormalParam

  @throws[CodeGeneratorException]
  def withFormalParams(thisType: ClassTypes, formalParams: List[VarDeclaration]): VariableTable = {
    val table = new VariableTable
    table.addEntry("this", thisType)
    formalParams.foreach(formalParam =>{
      table.addEntry(formalParam)
    })
    table
  }// withFormalParams

  @throws[CodeGeneratorException]
  def withFormalParamsFrom(thisType: ClassTypes, callable: MethodDef) = {
    withFormalParams(thisType, callable.parameters)
  }// withFormalParamsFrom

}
