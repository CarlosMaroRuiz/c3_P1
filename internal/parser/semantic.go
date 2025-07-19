package parser

import (
	"fmt"
	"js-analyzer/internal/lexer/types"
	"strings"
)

// SymbolType representa el tipo de símbolo
type SymbolType int

const (
	SYMBOL_UNDEFINED SymbolType = iota
	SYMBOL_VARIABLE
	SYMBOL_FUNCTION
	SYMBOL_PARAMETER
	SYMBOL_PROPERTY
)

func (st SymbolType) String() string {
	switch st {
	case SYMBOL_VARIABLE:
		return "VARIABLE"
	case SYMBOL_FUNCTION:
		return "FUNCTION"
	case SYMBOL_PARAMETER:
		return "PARAMETER"
	case SYMBOL_PROPERTY:
		return "PROPERTY"
	default:
		return "UNDEFINED"
	}
}

// VarScope representa el tipo de scope de variable
type VarScope int

const (
	SCOPE_VAR VarScope = iota
	SCOPE_LET
	SCOPE_CONST
	SCOPE_FUNCTION
	SCOPE_GLOBAL
)

func (vs VarScope) String() string {
	switch vs {
	case SCOPE_VAR:
		return "var"
	case SCOPE_LET:
		return "let"
	case SCOPE_CONST:
		return "const"
	case SCOPE_FUNCTION:
		return "function"
	case SCOPE_GLOBAL:
		return "global"
	default:
		return "unknown"
	}
}

// Symbol representa un símbolo en la tabla de símbolos
type Symbol struct {
	Name      string
	Type      SymbolType
	VarScope  VarScope
	Position  types.Position
	Declared  bool
	Used      bool
	Value     interface{}
	Scope     int // nivel de scope
	ReadOnly  bool // para const
}

// SymbolTable representa la tabla de símbolos
type SymbolTable struct {
	symbols map[string]*Symbol
	outer   *SymbolTable
	level   int
	name    string // nombre del scope (function, block, etc.)
}

// NewSymbolTable crea una nueva tabla de símbolos
func NewSymbolTable() *SymbolTable {
	return &SymbolTable{
		symbols: make(map[string]*Symbol),
		level:   0,
		name:    "global",
	}
}

// NewEnclosedSymbolTable crea una tabla de símbolos anidada
func NewEnclosedSymbolTable(outer *SymbolTable, name string) *SymbolTable {
	st := &SymbolTable{
		symbols: make(map[string]*Symbol),
		outer:   outer,
		level:   outer.level + 1,
		name:    name,
	}
	return st
}

// Define define un símbolo en la tabla
func (st *SymbolTable) Define(name string, symbolType SymbolType, varScope VarScope, pos types.Position) *Symbol {
	symbol := &Symbol{
		Name:     name,
		Type:     symbolType,
		VarScope: varScope,
		Position: pos,
		Declared: true,
		Used:     false,
		Scope:    st.level,
		ReadOnly: varScope == SCOPE_CONST,
	}
	st.symbols[name] = symbol
	return symbol
}

// Resolve resuelve un símbolo buscando en la tabla actual y las externas
func (st *SymbolTable) Resolve(name string) (*Symbol, bool) {
	symbol, ok := st.symbols[name]
	if !ok && st.outer != nil {
		symbol, ok = st.outer.Resolve(name)
	}
	if ok && symbol != nil {
		symbol.Used = true
	}
	return symbol, ok
}

// ResolveLocal resuelve un símbolo solo en la tabla actual
func (st *SymbolTable) ResolveLocal(name string) (*Symbol, bool) {
	symbol, ok := st.symbols[name]
	return symbol, ok
}

// GetAll retorna todos los símbolos de la tabla actual
func (st *SymbolTable) GetAll() map[string]*Symbol {
	return st.symbols
}

// GetName retorna el nombre del scope
func (st *SymbolTable) GetName() string {
	return st.name
}

// GetLevel retorna el nivel del scope
func (st *SymbolTable) GetLevel() int {
	return st.level
}

// SemanticError representa un error semántico
type SemanticError struct {
	Type     string
	Message  string
	Position types.Position
	Symbol   string
	Severity string // "error", "warning", "info"
}

func (se SemanticError) Error() string {
	return fmt.Sprintf("Semantic %s [%s] at line %d, column %d: %s", 
		se.Severity, se.Type, se.Position.Line, se.Position.Column, se.Message)
}

// SemanticAnalyzer realiza análisis semántico
type SemanticAnalyzer struct {
	symbolTable  *SymbolTable
	errors       []SemanticError
	currentScope int
	inFunction   bool
	functionName string
}

// NewSemanticAnalyzer crea un nuevo analizador semántico
func NewSemanticAnalyzer() *SemanticAnalyzer {
	return &SemanticAnalyzer{
		symbolTable:  NewSymbolTable(),
		errors:       []SemanticError{},
		currentScope: 0,
		inFunction:   false,
	}
}

// Analyze analiza el AST
func (sa *SemanticAnalyzer) Analyze(program *Program) {
	sa.analyzeProgram(program)
	sa.checkUnusedVariables()
}

// analyzeProgram analiza el programa
func (sa *SemanticAnalyzer) analyzeProgram(program *Program) {
	for _, stmt := range program.Statements {
		sa.analyzeStatement(stmt)
	}
}

// analyzeStatement analiza declaraciones
func (sa *SemanticAnalyzer) analyzeStatement(stmt Statement) {
	switch node := stmt.(type) {
	case *VarStatement:
		sa.analyzeVarStatement(node)
	case *FunctionStatement:
		sa.analyzeFunctionStatement(node)
	case *ReturnStatement:
		sa.analyzeReturnStatement(node)
	case *ExpressionStatement:
		sa.analyzeExpression(node.Expression)
	case *IfStatement:
		sa.analyzeIfStatement(node)
	case *WhileStatement:
		sa.analyzeWhileStatement(node)
	case *ForStatement:
		sa.analyzeForStatement(node)
	case *BlockStatement:
		sa.analyzeBlockStatement(node)
	}
}

// analyzeVarStatement analiza declaraciones de variables
func (sa *SemanticAnalyzer) analyzeVarStatement(stmt *VarStatement) {
	// Verificar si la variable ya está declarada en el scope actual para let/const
	if symbol, exists := sa.symbolTable.ResolveLocal(stmt.Name.Value); exists {
		if symbol.VarScope == SCOPE_LET || symbol.VarScope == SCOPE_CONST {
			sa.addError("REDECLARATION", 
				fmt.Sprintf("Variable '%s' already declared in this scope", stmt.Name.Value),
				stmt.Position, stmt.Name.Value, "error")
			return
		}
		// Para var, permitir redeclaración pero dar warning
		if symbol.VarScope == SCOPE_VAR && stmt.VarType == "var" {
			sa.addError("VAR_REDECLARATION", 
				fmt.Sprintf("Variable '%s' already declared with var", stmt.Name.Value),
				stmt.Position, stmt.Name.Value, "warning")
		}
	}

	// Determinar el tipo de scope
	var varScope VarScope
	switch stmt.VarType {
	case "var":
		varScope = SCOPE_VAR
	case "let":
		varScope = SCOPE_LET
	case "const":
		varScope = SCOPE_CONST
	}

	// Verificar que const tenga inicialización
	if varScope == SCOPE_CONST && stmt.Value == nil {
		sa.addError("CONST_WITHOUT_INIT", 
			"const declaration must have an initializer", 
			stmt.Position, stmt.Name.Value, "error")
	}

	// Definir el símbolo
	sa.symbolTable.Define(stmt.Name.Value, SYMBOL_VARIABLE, varScope, stmt.Position)

	// Analizar la expresión de valor si existe
	if stmt.Value != nil {
		sa.analyzeExpression(stmt.Value)
	}
}

// analyzeFunctionStatement analiza declaraciones de función
func (sa *SemanticAnalyzer) analyzeFunctionStatement(stmt *FunctionStatement) {
	// Verificar redeclaración de función
	if _, exists := sa.symbolTable.ResolveLocal(stmt.Name.Value); exists {
		sa.addError("FUNCTION_REDECLARATION", 
			fmt.Sprintf("Function '%s' already declared", stmt.Name.Value),
			stmt.Position, stmt.Name.Value, "error")
	}

	// Definir la función en la tabla de símbolos
	sa.symbolTable.Define(stmt.Name.Value, SYMBOL_FUNCTION, SCOPE_FUNCTION, stmt.Position)

	// Crear nuevo scope para la función
	enclosedTable := NewEnclosedSymbolTable(sa.symbolTable, "function:"+stmt.Name.Value)
	previousTable := sa.symbolTable
	sa.symbolTable = enclosedTable

	// Guardar estado de función
	previousInFunction := sa.inFunction
	previousFunctionName := sa.functionName
	sa.inFunction = true
	sa.functionName = stmt.Name.Value

	// Analizar parámetros
	paramNames := make(map[string]bool)
	for _, param := range stmt.Parameters {
		// Verificar parámetros duplicados
		if paramNames[param.Value] {
			sa.addError("DUPLICATE_PARAMETER", 
				fmt.Sprintf("Duplicate parameter '%s'", param.Value),
				param.Position, param.Value, "error")
		}
		paramNames[param.Value] = true

		// Definir parámetro en la tabla de símbolos
		sa.symbolTable.Define(param.Value, SYMBOL_PARAMETER, SCOPE_VAR, param.Position)
	}

	// Analizar el cuerpo de la función
	if stmt.Body != nil {
		sa.analyzeBlockStatement(stmt.Body)
	}

	// Restaurar estado anterior
	sa.symbolTable = previousTable
	sa.inFunction = previousInFunction
	sa.functionName = previousFunctionName
}

// analyzeReturnStatement analiza declaraciones return
func (sa *SemanticAnalyzer) analyzeReturnStatement(stmt *ReturnStatement) {
	if !sa.inFunction {
		sa.addError("RETURN_OUTSIDE_FUNCTION", 
			"return statement outside function", 
			stmt.Position, "", "error")
	}

	if stmt.ReturnValue != nil {
		sa.analyzeExpression(stmt.ReturnValue)
	}
}

// analyzeIfStatement analiza declaraciones if
func (sa *SemanticAnalyzer) analyzeIfStatement(stmt *IfStatement) {
	// Analizar condición
	sa.analyzeExpression(stmt.Condition)

	// Crear nuevo scope para el bloque if
	enclosedTable := NewEnclosedSymbolTable(sa.symbolTable, "if-block")
	previousTable := sa.symbolTable
	sa.symbolTable = enclosedTable

	// Analizar bloque de consecuencia
	sa.analyzeBlockStatement(stmt.Consequence)

	// Restaurar scope
	sa.symbolTable = previousTable

	// Analizar alternativa si existe
	if stmt.Alternative != nil {
		sa.analyzeStatement(stmt.Alternative)
	}
}

// analyzeWhileStatement analiza bucles while
func (sa *SemanticAnalyzer) analyzeWhileStatement(stmt *WhileStatement) {
	// Analizar condición
	sa.analyzeExpression(stmt.Condition)

	// Crear nuevo scope para el bucle
	enclosedTable := NewEnclosedSymbolTable(sa.symbolTable, "while-block")
	previousTable := sa.symbolTable
	sa.symbolTable = enclosedTable

	// Analizar cuerpo
	sa.analyzeBlockStatement(stmt.Body)

	// Restaurar scope
	sa.symbolTable = previousTable
}

// analyzeForStatement analiza bucles for
func (sa *SemanticAnalyzer) analyzeForStatement(stmt *ForStatement) {
	// Crear nuevo scope para el bucle for
	enclosedTable := NewEnclosedSymbolTable(sa.symbolTable, "for-block")
	previousTable := sa.symbolTable
	sa.symbolTable = enclosedTable

	// Analizar inicialización
	if stmt.Init != nil {
		sa.analyzeStatement(stmt.Init)
	}

	// Analizar condición
	if stmt.Condition != nil {
		sa.analyzeExpression(stmt.Condition)
	}

	// Analizar actualización
	if stmt.Update != nil {
		sa.analyzeExpression(stmt.Update)
	}

	// Analizar cuerpo
	sa.analyzeBlockStatement(stmt.Body)

	// Restaurar scope
	sa.symbolTable = previousTable
}

// analyzeBlockStatement analiza bloques de declaraciones
func (sa *SemanticAnalyzer) analyzeBlockStatement(stmt *BlockStatement) {
	// Crear nuevo scope para el bloque (solo para let/const)
	enclosedTable := NewEnclosedSymbolTable(sa.symbolTable, "block")
	previousTable := sa.symbolTable
	sa.symbolTable = enclosedTable

	// Analizar todas las declaraciones del bloque
	for _, s := range stmt.Statements {
		sa.analyzeStatement(s)
	}

	// Restaurar scope anterior
	sa.symbolTable = previousTable
}

// analyzeExpression analiza expresiones
func (sa *SemanticAnalyzer) analyzeExpression(expr Expression) {
	if expr == nil {
		return
	}

	switch node := expr.(type) {
	case *Identifier:
		sa.analyzeIdentifier(node)
	case *InfixExpression:
		sa.analyzeInfixExpression(node)
	case *PrefixExpression:
		sa.analyzePrefixExpression(node)
	case *PostfixExpression:
		sa.analyzePostfixExpression(node)
	case *CallExpression:
		sa.analyzeCallExpression(node)
	case *AssignmentExpression:
		sa.analyzeAssignmentExpression(node)
	case *MemberExpression:
		sa.analyzeMemberExpression(node)
	case *ArrayLiteral:
		sa.analyzeArrayLiteral(node)
	case *ObjectLiteral:
		sa.analyzeObjectLiteral(node)
	case *IntegerLiteral, *FloatLiteral, *StringLiteral, *BooleanLiteral, *NullLiteral, *TemplateLiteral:
		// Literales no necesitan análisis adicional
		return
	}
}

// analyzeIdentifier analiza identificadores
func (sa *SemanticAnalyzer) analyzeIdentifier(ident *Identifier) {
	symbol, exists := sa.symbolTable.Resolve(ident.Value)
	if !exists {
		sa.addError("UNDEFINED_VARIABLE", 
			fmt.Sprintf("Variable '%s' is not defined", ident.Value),
			ident.Position, ident.Value, "error")
		return
	}

	// Marcar como usado (ya se hace en Resolve, pero por claridad)
	symbol.Used = true
}

// analyzeInfixExpression analiza expresiones infix
func (sa *SemanticAnalyzer) analyzeInfixExpression(expr *InfixExpression) {
	sa.analyzeExpression(expr.Left)
	sa.analyzeExpression(expr.Right)

	// Verificar tipos compatibles (análisis básico)
	sa.checkTypeCompatibility(expr.Left, expr.Right, expr.Operator, expr.Position)
}

// analyzePrefixExpression analiza expresiones prefix
func (sa *SemanticAnalyzer) analyzePrefixExpression(expr *PrefixExpression) {
	sa.analyzeExpression(expr.Right)

	// Verificar operadores prefix específicos
	if expr.Operator == "++" || expr.Operator == "--" {
		// Verificar que la expresión sea asignable
		if !sa.isAssignable(expr.Right) {
			sa.addError("INVALID_INCREMENT", 
				fmt.Sprintf("Cannot apply %s to non-assignable expression", expr.Operator),
				expr.Position, "", "error")
		}
	}
}

// analyzePostfixExpression analiza expresiones postfix
func (sa *SemanticAnalyzer) analyzePostfixExpression(expr *PostfixExpression) {
	sa.analyzeExpression(expr.Left)

	// Verificar que la expresión sea asignable
	if !sa.isAssignable(expr.Left) {
		sa.addError("INVALID_INCREMENT", 
			fmt.Sprintf("Cannot apply %s to non-assignable expression", expr.Operator),
			expr.Position, "", "error")
	}
}

// analyzeCallExpression analiza llamadas a función
func (sa *SemanticAnalyzer) analyzeCallExpression(expr *CallExpression) {
	sa.analyzeExpression(expr.Function)

	// Verificar que sea una función
	if ident, ok := expr.Function.(*Identifier); ok {
		if symbol, exists := sa.symbolTable.Resolve(ident.Value); exists {
			if symbol.Type != SYMBOL_FUNCTION {
				sa.addError("NOT_A_FUNCTION", 
					fmt.Sprintf("'%s' is not a function", ident.Value),
					expr.Position, ident.Value, "error")
			}
		}
	}

	// Analizar argumentos
	for _, arg := range expr.Arguments {
		sa.analyzeExpression(arg)
	}
}

// analyzeAssignmentExpression analiza asignaciones
func (sa *SemanticAnalyzer) analyzeAssignmentExpression(expr *AssignmentExpression) {
	// Verificar que la variable esté declarada
	symbol, exists := sa.symbolTable.Resolve(expr.Name.Value)
	if !exists {
		sa.addError("UNDEFINED_VARIABLE", 
			fmt.Sprintf("Variable '%s' is not defined", expr.Name.Value),
			expr.Position, expr.Name.Value, "error")
		return
	}

	// Verificar que no sea const
	if symbol.ReadOnly {
		sa.addError("CONST_ASSIGNMENT", 
			fmt.Sprintf("Cannot assign to const variable '%s'", expr.Name.Value),
			expr.Position, expr.Name.Value, "error")
	}

	// Marcar como usado
	symbol.Used = true

	// Analizar el valor
	sa.analyzeExpression(expr.Value)
}

// analyzeMemberExpression analiza acceso a miembros
func (sa *SemanticAnalyzer) analyzeMemberExpression(expr *MemberExpression) {
	sa.analyzeExpression(expr.Object)
	sa.analyzeExpression(expr.Property)
}

// analyzeArrayLiteral analiza arrays literales
func (sa *SemanticAnalyzer) analyzeArrayLiteral(expr *ArrayLiteral) {
	for _, element := range expr.Elements {
		sa.analyzeExpression(element)
	}
}

// analyzeObjectLiteral analiza objetos literales
func (sa *SemanticAnalyzer) analyzeObjectLiteral(expr *ObjectLiteral) {
	for key, value := range expr.Pairs {
		sa.analyzeExpression(key)
		sa.analyzeExpression(value)
	}
}

// checkTypeCompatibility verifica compatibilidad de tipos básica
func (sa *SemanticAnalyzer) checkTypeCompatibility(left, right Expression, operator string, pos types.Position) {
	leftType := sa.getExpressionType(left)
	rightType := sa.getExpressionType(right)

	// Verificaciones básicas
	switch operator {
	case "+":
		// + puede ser suma o concatenación, generalmente válido
	case "-", "*", "/", "%", "**":
		if leftType == "string" || rightType == "string" {
			sa.addError("TYPE_MISMATCH", 
				fmt.Sprintf("Cannot apply arithmetic operator '%s' to string", operator),
				pos, "", "warning")
		}
	case "&&", "||":
		// Los operadores lógicos pueden trabajar con cualquier tipo en JS
	case "==", "!=":
		// Comparaciones con coerción son válidas pero pueden dar warning
		if leftType != rightType && leftType != "unknown" && rightType != "unknown" {
			sa.addError("TYPE_COERCION", 
				fmt.Sprintf("Comparing %s with %s may cause type coercion", leftType, rightType),
				pos, "", "info")
		}
	case "===", "!==":
		// Comparaciones estrictas son siempre válidas
	case "<", ">", "<=", ">=":
		// Comparaciones numéricas
		if (leftType == "string" && rightType != "string") || 
		   (leftType != "string" && rightType == "string") {
			sa.addError("TYPE_MISMATCH", 
				fmt.Sprintf("Comparing %s with %s", leftType, rightType),
				pos, "", "warning")
		}
	}
}

// getExpressionType obtiene el tipo básico de una expresión
func (sa *SemanticAnalyzer) getExpressionType(expr Expression) string {
	switch expr.(type) {
	case *IntegerLiteral, *FloatLiteral:
		return "number"
	case *StringLiteral, *TemplateLiteral:
		return "string"
	case *BooleanLiteral:
		return "boolean"
	case *NullLiteral:
		return "null"
	case *ArrayLiteral:
		return "array"
	case *ObjectLiteral:
		return "object"
	case *Identifier:
		// En un analizador más complejo, podríamos rastrear tipos
		return "unknown"
	default:
		return "unknown"
	}
}

// isAssignable verifica si una expresión es asignable
func (sa *SemanticAnalyzer) isAssignable(expr Expression) bool {
	switch expr.(type) {
	case *Identifier:
		return true
	case *MemberExpression:
		return true
	default:
		return false
	}
}

// checkUnusedVariables verifica variables no utilizadas
func (sa *SemanticAnalyzer) checkUnusedVariables() {
	sa.checkUnusedInTable(sa.symbolTable)
}

// checkUnusedInTable verifica variables no utilizadas en una tabla
func (sa *SemanticAnalyzer) checkUnusedInTable(table *SymbolTable) {
	for name, symbol := range table.GetAll() {
		if symbol.Type == SYMBOL_VARIABLE && !symbol.Used {
			severity := "warning"
			if symbol.VarScope == SCOPE_CONST {
				severity = "error" // const no usado es más grave
			}
			sa.addError("UNUSED_VARIABLE", 
				fmt.Sprintf("Variable '%s' is declared but never used", name),
				symbol.Position, name, severity)
		}
		if symbol.Type == SYMBOL_FUNCTION && !symbol.Used {
			sa.addError("UNUSED_FUNCTION", 
				fmt.Sprintf("Function '%s' is declared but never used", name),
				symbol.Position, name, "warning")
		}
	}
}

// addError añade un error semántico
func (sa *SemanticAnalyzer) addError(errorType, message string, pos types.Position, symbol, severity string) {
	sa.errors = append(sa.errors, SemanticError{
		Type:     errorType,
		Message:  message,
		Position: pos,
		Symbol:   symbol,
		Severity: severity,
	})
}

// GetErrors retorna todos los errores semánticos
func (sa *SemanticAnalyzer) GetErrors() []SemanticError {
	return sa.errors
}

// GetErrorsByType retorna errores filtrados por tipo
func (sa *SemanticAnalyzer) GetErrorsByType(errorType string) []SemanticError {
	var filtered []SemanticError
	for _, err := range sa.errors {
		if err.Type == errorType {
			filtered = append(filtered, err)
		}
	}
	return filtered
}

// GetErrorsBySeverity retorna errores filtrados por severidad
func (sa *SemanticAnalyzer) GetErrorsBySeverity(severity string) []SemanticError {
	var filtered []SemanticError
	for _, err := range sa.errors {
		if err.Severity == severity {
			filtered = append(filtered, err)
		}
	}
	return filtered
}

// GetSymbolTable retorna la tabla de símbolos
func (sa *SemanticAnalyzer) GetSymbolTable() *SymbolTable {
	return sa.symbolTable
}

// HasErrors verifica si hay errores
func (sa *SemanticAnalyzer) HasErrors() bool {
	return len(sa.errors) > 0
}

// HasErrorsOfSeverity verifica si hay errores de una severidad específica
func (sa *SemanticAnalyzer) HasErrorsOfSeverity(severity string) bool {
	for _, err := range sa.errors {
		if err.Severity == severity {
			return true
		}
	}
	return false
}

// GetErrorSummary retorna un resumen de errores
func (sa *SemanticAnalyzer) GetErrorSummary() string {
	if len(sa.errors) == 0 {
		return "No semantic errors found"
	}

	var builder strings.Builder
	builder.WriteString(fmt.Sprintf("Found %d semantic issue(s):\n", len(sa.errors)))
	
	// Contar por severidad
	errorCount := len(sa.GetErrorsBySeverity("error"))
	warningCount := len(sa.GetErrorsBySeverity("warning"))
	infoCount := len(sa.GetErrorsBySeverity("info"))
	
	if errorCount > 0 {
		builder.WriteString(fmt.Sprintf("- Errors: %d\n", errorCount))
	}
	if warningCount > 0 {
		builder.WriteString(fmt.Sprintf("- Warnings: %d\n", warningCount))
	}
	if infoCount > 0 {
		builder.WriteString(fmt.Sprintf("- Info: %d\n", infoCount))
	}
	
	builder.WriteString("\nDetails:\n")
	for i, err := range sa.errors {
		builder.WriteString(fmt.Sprintf("%d. %s\n", i+1, err.Error()))
	}
	
	return builder.String()
}

// AnalysisResult representa el resultado del análisis semántico
type AnalysisResult struct {
	Errors      []SemanticError
	SymbolTable *SymbolTable
	Summary     string
	HasErrors   bool
	Statistics  AnalysisStatistics
}

// AnalysisStatistics representa estadísticas del análisis
type AnalysisStatistics struct {
	TotalSymbols    int
	Variables       int
	Functions       int
	Parameters      int
	UnusedVariables int
	UnusedFunctions int
	ErrorCount      int
	WarningCount    int
	InfoCount       int
}

// GetAnalysisResult retorna el resultado completo del análisis
func (sa *SemanticAnalyzer) GetAnalysisResult() AnalysisResult {
	stats := sa.calculateStatistics()
	
	return AnalysisResult{
		Errors:      sa.errors,
		SymbolTable: sa.symbolTable,
		Summary:     sa.GetErrorSummary(),
		HasErrors:   sa.HasErrorsOfSeverity("error"),
		Statistics:  stats,
	}
}

// calculateStatistics calcula estadísticas del análisis
func (sa *SemanticAnalyzer) calculateStatistics() AnalysisStatistics {
	stats := AnalysisStatistics{}
	
	// Contar símbolos
	sa.countSymbolsInTable(sa.symbolTable, &stats)
	
	// Contar errores por severidad
	stats.ErrorCount = len(sa.GetErrorsBySeverity("error"))
	stats.WarningCount = len(sa.GetErrorsBySeverity("warning"))
	stats.InfoCount = len(sa.GetErrorsBySeverity("info"))
	
	// Contar no utilizados
	stats.UnusedVariables = len(sa.GetErrorsByType("UNUSED_VARIABLE"))
	stats.UnusedFunctions = len(sa.GetErrorsByType("UNUSED_FUNCTION"))
	
	return stats
}

// countSymbolsInTable cuenta símbolos recursivamente
func (sa *SemanticAnalyzer) countSymbolsInTable(table *SymbolTable, stats *AnalysisStatistics) {
	for _, symbol := range table.GetAll() {
		stats.TotalSymbols++
		switch symbol.Type {
		case SYMBOL_VARIABLE:
			stats.Variables++
		case SYMBOL_FUNCTION:
			stats.Functions++
		case SYMBOL_PARAMETER:
			stats.Parameters++
		}
	}
}

// Reset reinicia el analizador semántico
func (sa *SemanticAnalyzer) Reset() {
	sa.symbolTable = NewSymbolTable()
	sa.errors = []SemanticError{}
	sa.currentScope = 0
	sa.inFunction = false
	sa.functionName = ""
}