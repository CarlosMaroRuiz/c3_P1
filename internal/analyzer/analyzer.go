package analyzer

import (
	"fmt"
	"js-analyzer/internal/lexer"
	"js-analyzer/internal/lexer/types"
	"js-analyzer/internal/parser"
	"strings"
	"time"
)

// AnalysisType representa el tipo de análisis a realizar
type AnalysisType int

const (
	LEXICAL_ONLY AnalysisType = iota
	SYNTACTIC_ONLY
	SEMANTIC_ONLY
	FULL_ANALYSIS
)

// String retorna la representación en string del tipo de análisis
func (at AnalysisType) String() string {
	switch at {
	case LEXICAL_ONLY:
		return "Lexical Only"
	case SYNTACTIC_ONLY:
		return "Syntactic Only"
	case SEMANTIC_ONLY:
		return "Semantic Only"
	case FULL_ANALYSIS:
		return "Full Analysis"
	default:
		return "Unknown"
	}
}

// AnalysisRequest representa una solicitud de análisis
type AnalysisRequest struct {
	Code         string      `json:"code"`
	AnalysisType AnalysisType `json:"analysisType"`
	ShowTokens   bool        `json:"showTokens"`
	ShowAST      bool        `json:"showAST"`
	ShowSymbols  bool        `json:"showSymbols"`
	Options      AnalysisOptions `json:"options"`
}

// AnalysisOptions representa opciones adicionales para el análisis
type AnalysisOptions struct {
	SkipComments     bool `json:"skipComments"`
	StrictMode       bool `json:"strictMode"`
	MaxTokenLength   int  `json:"maxTokenLength"`
	EnableOptimizations bool `json:"enableOptimizations"`
}

// TokenInfo representa información de un token para la respuesta
type TokenInfo struct {
	Type     string         `json:"type"`
	Literal  string         `json:"literal"`
	Position types.Position `json:"position"`
	Category string         `json:"category"`
	IsKeyword bool          `json:"isKeyword"`
	IsOperator bool         `json:"isOperator"`
	IsLiteral bool          `json:"isLiteral"`
}

// ASTInfo representa información del AST
type ASTInfo struct {
	NodeType string         `json:"nodeType"`
	Content  string         `json:"content"`
	Position types.Position `json:"position"`
	Children []ASTInfo      `json:"children,omitempty"`
	Properties map[string]interface{} `json:"properties,omitempty"`
}

// SymbolInfo representa información de símbolos
type SymbolInfo struct {
	Name     string         `json:"name"`
	Type     string         `json:"type"`
	Scope    string         `json:"scope"`
	Position types.Position `json:"position"`
	Used     bool           `json:"used"`
	Declared bool           `json:"declared"`
	ReadOnly bool           `json:"readOnly"`
	Level    int            `json:"level"`
}

// ErrorInfo representa información de errores
type ErrorInfo struct {
	Type     string         `json:"type"`
	Message  string         `json:"message"`
	Position types.Position `json:"position"`
	Severity string         `json:"severity"`
	Symbol   string         `json:"symbol,omitempty"`
	Code     string         `json:"code,omitempty"`
}

// AnalysisResponse representa la respuesta del análisis
type AnalysisResponse struct {
	Success         bool            `json:"success"`
	ExecutionTime   time.Duration   `json:"executionTime"`
	TokenCount      int             `json:"tokenCount"`
	LineCount       int             `json:"lineCount"`
	AnalysisType    string          `json:"analysisType"`
	
	// Resultados del análisis
	Tokens          []TokenInfo     `json:"tokens,omitempty"`
	AST             *ASTInfo        `json:"ast,omitempty"`
	Symbols         []SymbolInfo    `json:"symbols,omitempty"`
	
	// Errores
	LexicalErrors   []ErrorInfo     `json:"lexicalErrors,omitempty"`
	SyntacticErrors []ErrorInfo     `json:"syntacticErrors,omitempty"`
	SemanticErrors  []ErrorInfo     `json:"semanticErrors,omitempty"`
	
	// Estadísticas
	Statistics      Statistics      `json:"statistics"`
	
	// Mensajes
	Summary         string          `json:"summary"`
	Warnings        []string        `json:"warnings,omitempty"`
}

// Statistics representa estadísticas del análisis
type Statistics struct {
	Variables       int     `json:"variables"`
	Functions       int     `json:"functions"`
	Parameters      int     `json:"parameters"`
	Statements      int     `json:"statements"`
	Expressions     int     `json:"expressions"`
	MaxNesting      int     `json:"maxNesting"`
	Complexity      int     `json:"complexity"`
	UnusedVariables int     `json:"unusedVariables"`
	UnusedFunctions int     `json:"unusedFunctions"`
	ErrorCount      int     `json:"errorCount"`
	WarningCount    int     `json:"warningCount"`
	InfoCount       int     `json:"infoCount"`
	CoveragePercent float64 `json:"coveragePercent"`
}

// JSAnalyzer es el analizador principal optimizado
type JSAnalyzer struct {
	config AnalyzerConfig
}

// AnalyzerConfig contiene configuración del analizador
type AnalyzerConfig struct {
	MaxTokens        int
	MaxNesting       int
	TimeoutSeconds   int
	EnableCache      bool
	EnableStatistics bool
	VerboseLogging   bool
}

// NewJSAnalyzer crea un nuevo analizador con configuración por defecto
func NewJSAnalyzer() *JSAnalyzer {
	return &JSAnalyzer{
		config: AnalyzerConfig{
			MaxTokens:        10000,
			MaxNesting:       50,
			TimeoutSeconds:   30,
			EnableCache:      true,
			EnableStatistics: true,
			VerboseLogging:   false,
		},
	}
}

// NewJSAnalyzerWithConfig crea un analizador con configuración personalizada
func NewJSAnalyzerWithConfig(config AnalyzerConfig) *JSAnalyzer {
	return &JSAnalyzer{config: config}
}

// Analyze realiza el análisis completo del código JavaScript
func (ja *JSAnalyzer) Analyze(request AnalysisRequest) *AnalysisResponse {
	startTime := time.Now()
	
	response := &AnalysisResponse{
		Success:         true,
		AnalysisType:    request.AnalysisType.String(),
		LexicalErrors:   []ErrorInfo{},
		SyntacticErrors: []ErrorInfo{},
		SemanticErrors:  []ErrorInfo{},
		Statistics:      Statistics{},
		Warnings:        []string{},
	}

	// Validar entrada
	if strings.TrimSpace(request.Code) == "" {
		response.Success = false
		response.Summary = "Error: Empty code provided"
		response.ExecutionTime = time.Since(startTime)
		return response
	}

	// Validar configuración
	if err := ja.validateRequest(request, response); err != nil {
		response.Success = false
		response.Summary = fmt.Sprintf("Configuration error: %s", err.Error())
		response.ExecutionTime = time.Since(startTime)
		return response
	}

	// Contar líneas
	response.LineCount = strings.Count(request.Code, "\n") + 1

	var program *parser.Program
	var lexerInstance lexer.Lexer

	// Fase 1: Análisis Léxico
	if request.AnalysisType == LEXICAL_ONLY || request.AnalysisType == FULL_ANALYSIS {
		lexerInstance = ja.performLexicalAnalysis(request, response)
		if len(response.LexicalErrors) > 0 && request.AnalysisType == LEXICAL_ONLY {
			response.Success = ja.shouldContinue(response.LexicalErrors)
			response.ExecutionTime = time.Since(startTime)
			ja.generateSummary(response)
			return response
		}
	}

	// Fase 2: Análisis Sintáctico
	if request.AnalysisType == SYNTACTIC_ONLY || request.AnalysisType == FULL_ANALYSIS {
		if lexerInstance == nil {
			lexerInstance = lexer.New(request.Code)
		}
		
		program = ja.performSyntacticAnalysis(lexerInstance, response)
		if len(response.SyntacticErrors) > 0 && request.AnalysisType == SYNTACTIC_ONLY {
			response.Success = ja.shouldContinue(response.SyntacticErrors)
			response.ExecutionTime = time.Since(startTime)
			ja.generateSummary(response)
			return response
		}

		// Generar información del AST si se solicita
		if request.ShowAST && program != nil {
			response.AST = ja.generateASTInfo(program)
		}
	}

	// Fase 3: Análisis Semántico
	if (request.AnalysisType == SEMANTIC_ONLY || request.AnalysisType == FULL_ANALYSIS) && program != nil {
		ja.performSemanticAnalysis(program, request, response)
	}

	// Generar estadísticas si está habilitado
	if ja.config.EnableStatistics {
		ja.generateStatistics(response, program)
	}

	// Generar resumen
	ja.generateSummary(response)
	
	response.ExecutionTime = time.Since(startTime)
	
	// Determinar éxito basado en errores críticos
	response.Success = ja.determineSuccess(response)

	return response
}

// validateRequest valida la solicitud de análisis
func (ja *JSAnalyzer) validateRequest(request AnalysisRequest, response *AnalysisResponse) error {
	if len(request.Code) > 1000000 { // 1MB límite
		return fmt.Errorf("code size exceeds maximum limit (1MB)")
	}

	if request.Options.MaxTokenLength > 0 && request.Options.MaxTokenLength > 10000 {
		response.Warnings = append(response.Warnings, "MaxTokenLength is very high, may affect performance")
	}

	return nil
}

// performLexicalAnalysis realiza el análisis léxico
func (ja *JSAnalyzer) performLexicalAnalysis(request AnalysisRequest, response *AnalysisResponse) lexer.Lexer {
	// Crear configuración del lexer basada en las opciones
	lexerConfig := ja.createLexerConfig(request.Options)
	
	var l lexer.Lexer
	if lexerConfig != nil {
		// Si tenemos configuración específica, usar el core lexer
		l = lexer.New(request.Code) // Por ahora usamos el lexer estándar
	} else {
		l = lexer.New(request.Code)
	}
	
	tokens := l.GetAllTokens()
	response.TokenCount = len(tokens)

	// Procesar tokens si se solicita
	if request.ShowTokens {
		response.Tokens = make([]TokenInfo, 0, len(tokens))
		for _, token := range tokens {
			if token.Type == lexer.EOF {
				break
			}
			
			if token.Type == lexer.ILLEGAL {
				response.LexicalErrors = append(response.LexicalErrors, ErrorInfo{
					Type:     "ILLEGAL_TOKEN",
					Message:  fmt.Sprintf("Illegal token: %s", token.Literal),
					Position: token.Position,
					Severity: "error",
				})
			}

			tokenInfo := TokenInfo{
				Type:     token.Type.String(),
				Literal:  token.Literal,
				Position: token.Position,
				Category: token.Type.GetCategory(),
				IsKeyword: token.IsKeyword(),
				IsOperator: token.IsOperator(),
				IsLiteral: token.IsLiteral(),
			}

			response.Tokens = append(response.Tokens, tokenInfo)
		}
	}

	// Verificar límite de tokens
	if len(tokens) > ja.config.MaxTokens {
		response.LexicalErrors = append(response.LexicalErrors, ErrorInfo{
			Type:     "TOKEN_LIMIT_EXCEEDED",
			Message:  fmt.Sprintf("Token count (%d) exceeds maximum allowed (%d)", len(tokens), ja.config.MaxTokens),
			Position: types.Position{},
			Severity: "warning",
		})
	}

	return l
}

// createLexerConfig crea configuración del lexer
func (ja *JSAnalyzer) createLexerConfig(options AnalysisOptions) interface{} {
	// Por ahora retornamos nil, pero aquí podríamos crear configuración específica
	// cuando implementemos configuración avanzada del lexer
	return nil
}

// performSyntacticAnalysis realiza el análisis sintáctico
func (ja *JSAnalyzer) performSyntacticAnalysis(l lexer.Lexer, response *AnalysisResponse) *parser.Program {
	p := parser.New(l)
	program := p.ParseProgram()
	
	// Procesar errores de parsing
	for _, err := range p.Errors() {
		response.SyntacticErrors = append(response.SyntacticErrors, ErrorInfo{
			Type:     "PARSE_ERROR",
			Message:  err,
			Position: types.Position{}, // Los errores básicos no tienen posición específica
			Severity: "error",
		})
	}

	// Procesar errores detallados si están disponibles
	for _, err := range p.GetDetailedErrors() {
		response.SyntacticErrors = append(response.SyntacticErrors, ErrorInfo{
			Type:     "DETAILED_PARSE_ERROR",
			Message:  err.Message,
			Position: err.Token.Position,
			Severity: "error",
		})
	}

	return program
}

// performSemanticAnalysis realiza el análisis semántico
func (ja *JSAnalyzer) performSemanticAnalysis(program *parser.Program, request AnalysisRequest, response *AnalysisResponse) {
	sa := parser.NewSemanticAnalyzer()
	sa.Analyze(program)

	// Procesar errores semánticos
	for _, err := range sa.GetErrors() {
		response.SemanticErrors = append(response.SemanticErrors, ErrorInfo{
			Type:     err.Type,
			Message:  err.Message,
			Position: err.Position,
			Severity: err.Severity,
			Symbol:   err.Symbol,
		})
	}

	// Generar información de símbolos si se solicita
	if request.ShowSymbols {
		symbolTable := sa.GetSymbolTable()
		response.Symbols = ja.generateSymbolInfo(symbolTable)
	}
}

// generateASTInfo genera información del AST de forma recursiva
func (ja *JSAnalyzer) generateASTInfo(node parser.Node) *ASTInfo {
	if node == nil {
		return nil
	}

	info := &ASTInfo{
		NodeType:   node.Type(),
		Content:    node.String(),
		Position:   node.GetPosition(),
		Children:   []ASTInfo{},
		Properties: make(map[string]interface{}),
	}

	// Agregar propiedades específicas según el tipo de nodo
	switch n := node.(type) {
	case *parser.Program:
		info.Properties["statementCount"] = len(n.Statements)
		for _, stmt := range n.Statements {
			if child := ja.generateASTInfo(stmt); child != nil {
				info.Children = append(info.Children, *child)
			}
		}
		
	case *parser.VarStatement:
		info.Properties["varType"] = n.VarType
		info.Properties["hasInitializer"] = n.Value != nil
		if n.Name != nil {
			if child := ja.generateASTInfo(n.Name); child != nil {
				info.Children = append(info.Children, *child)
			}
		}
		if n.Value != nil {
			if child := ja.generateASTInfo(n.Value); child != nil {
				info.Children = append(info.Children, *child)
			}
		}
		
	case *parser.FunctionStatement:
		info.Properties["isAsync"] = n.IsAsync
		info.Properties["isArrow"] = n.IsArrow
		info.Properties["parameterCount"] = len(n.Parameters)
		if n.Name != nil {
			if child := ja.generateASTInfo(n.Name); child != nil {
				info.Children = append(info.Children, *child)
			}
		}
		for _, param := range n.Parameters {
			if child := ja.generateASTInfo(param); child != nil {
				info.Children = append(info.Children, *child)
			}
		}
		if n.Body != nil {
			if child := ja.generateASTInfo(n.Body); child != nil {
				info.Children = append(info.Children, *child)
			}
		}
		
	case *parser.BlockStatement:
		info.Properties["statementCount"] = len(n.Statements)
		for _, stmt := range n.Statements {
			if child := ja.generateASTInfo(stmt); child != nil {
				info.Children = append(info.Children, *child)
			}
		}
		
	case *parser.InfixExpression:
		info.Properties["operator"] = n.Operator
		if n.Left != nil {
			if child := ja.generateASTInfo(n.Left); child != nil {
				info.Children = append(info.Children, *child)
			}
		}
		if n.Right != nil {
			if child := ja.generateASTInfo(n.Right); child != nil {
				info.Children = append(info.Children, *child)
			}
		}
		
	case *parser.CallExpression:
		info.Properties["argumentCount"] = len(n.Arguments)
		if n.Function != nil {
			if child := ja.generateASTInfo(n.Function); child != nil {
				info.Children = append(info.Children, *child)
			}
		}
		for _, arg := range n.Arguments {
			if child := ja.generateASTInfo(arg); child != nil {
				info.Children = append(info.Children, *child)
			}
		}
		
	case *parser.ArrayLiteral:
		info.Properties["elementCount"] = len(n.Elements)
		for _, element := range n.Elements {
			if child := ja.generateASTInfo(element); child != nil {
				info.Children = append(info.Children, *child)
			}
		}
		
	case *parser.ObjectLiteral:
		info.Properties["propertyCount"] = len(n.Pairs)
		for key, value := range n.Pairs {
			if child := ja.generateASTInfo(key); child != nil {
				info.Children = append(info.Children, *child)
			}
			if child := ja.generateASTInfo(value); child != nil {
				info.Children = append(info.Children, *child)
			}
		}
		
	case *parser.IntegerLiteral:
		info.Properties["value"] = n.Value
		
	case *parser.FloatLiteral:
		info.Properties["value"] = n.Value
		
	case *parser.StringLiteral:
		info.Properties["value"] = n.Value
		info.Properties["length"] = len(n.Value)
		
	case *parser.BooleanLiteral:
		info.Properties["value"] = n.Value
		
	case *parser.Identifier:
		info.Properties["name"] = n.Value
	}

	return info
}

// generateSymbolInfo genera información de la tabla de símbolos
func (ja *JSAnalyzer) generateSymbolInfo(symbolTable *parser.SymbolTable) []SymbolInfo {
	symbols := []SymbolInfo{}
	
	for name, symbol := range symbolTable.GetAll() {
		symbolInfo := SymbolInfo{
			Name:     name,
			Type:     symbol.Type.String(),
			Scope:    symbol.VarScope.String(),
			Position: symbol.Position,
			Used:     symbol.Used,
			Declared: symbol.Declared,
			ReadOnly: symbol.ReadOnly,
			Level:    symbol.Scope,
		}
		symbols = append(symbols, symbolInfo)
	}
	
	return symbols
}

// generateStatistics genera estadísticas del análisis
func (ja *JSAnalyzer) generateStatistics(response *AnalysisResponse, program *parser.Program) {
	stats := &response.Statistics
	
	// Estadísticas básicas de errores
	stats.ErrorCount = len(response.LexicalErrors) + len(response.SyntacticErrors)
	for _, err := range response.SemanticErrors {
		switch err.Severity {
		case "error":
			stats.ErrorCount++
		case "warning":
			stats.WarningCount++
		case "info":
			stats.InfoCount++
		}
	}
	
	// Estadísticas de símbolos
	if len(response.Symbols) > 0 {
		for _, symbol := range response.Symbols {
			switch symbol.Type {
			case "VARIABLE":
				stats.Variables++
				if !symbol.Used {
					stats.UnusedVariables++
				}
			case "FUNCTION":
				stats.Functions++
				if !symbol.Used {
					stats.UnusedFunctions++
				}
			case "PARAMETER":
				stats.Parameters++
			}
		}
	}
	
	// Estadísticas del AST
	if program != nil {
		stats.Statements = len(program.Statements)
		stats.MaxNesting = ja.calculateMaxNesting(program)
		stats.Complexity = ja.calculateComplexity(program)
	}
	
	// Calcular porcentaje de cobertura (variables usadas vs declaradas)
	if stats.Variables > 0 {
		usedVars := stats.Variables - stats.UnusedVariables
		stats.CoveragePercent = float64(usedVars) / float64(stats.Variables) * 100
	}
}

// calculateMaxNesting calcula el nivel máximo de anidamiento
func (ja *JSAnalyzer) calculateMaxNesting(node parser.Node) int {
	maxNesting := 0
	
	switch n := node.(type) {
	case *parser.Program:
		for _, stmt := range n.Statements {
			if nesting := ja.calculateMaxNesting(stmt); nesting > maxNesting {
				maxNesting = nesting
			}
		}
	case *parser.BlockStatement:
		currentNesting := 1
		for _, stmt := range n.Statements {
			if nesting := 1 + ja.calculateMaxNesting(stmt); nesting > maxNesting {
				maxNesting = nesting
			}
		}
		if currentNesting > maxNesting {
			maxNesting = currentNesting
		}
	case *parser.IfStatement:
		nesting := 1 + ja.calculateMaxNesting(n.Consequence)
		if n.Alternative != nil {
			if altNesting := 1 + ja.calculateMaxNesting(n.Alternative); altNesting > nesting {
				nesting = altNesting
			}
		}
		if nesting > maxNesting {
			maxNesting = nesting
		}
	case *parser.FunctionStatement:
		if n.Body != nil {
			if nesting := 1 + ja.calculateMaxNesting(n.Body); nesting > maxNesting {
				maxNesting = nesting
			}
		}
	}
	
	return maxNesting
}

// calculateComplexity calcula la complejidad ciclomática básica
func (ja *JSAnalyzer) calculateComplexity(node parser.Node) int {
	complexity := 1 // Base complexity
	
	switch n := node.(type) {
	case *parser.Program:
		for _, stmt := range n.Statements {
			complexity += ja.calculateComplexity(stmt)
		}
	case *parser.IfStatement:
		complexity++
		complexity += ja.calculateComplexity(n.Consequence)
		if n.Alternative != nil {
			complexity += ja.calculateComplexity(n.Alternative)
		}
	case *parser.WhileStatement:
		complexity++
		complexity += ja.calculateComplexity(n.Body)
	case *parser.ForStatement:
		complexity++
		complexity += ja.calculateComplexity(n.Body)
	case *parser.BlockStatement:
		for _, stmt := range n.Statements {
			complexity += ja.calculateComplexity(stmt)
		}
	}
	
	return complexity - 1 // Ajustar por la complejidad base
}

// shouldContinue determina si el análisis debe continuar basado en los errores
func (ja *JSAnalyzer) shouldContinue(errors []ErrorInfo) bool {
	criticalErrors := 0
	for _, err := range errors {
		if err.Severity == "error" {
			criticalErrors++
		}
	}
	return criticalErrors == 0
}

// determineSuccess determina el éxito del análisis
func (ja *JSAnalyzer) determineSuccess(response *AnalysisResponse) bool {
	// El análisis es exitoso si no hay errores críticos
	criticalErrors := 0
	
	for _, err := range response.LexicalErrors {
		if err.Severity == "error" {
			criticalErrors++
		}
	}
	
	for _, err := range response.SyntacticErrors {
		if err.Severity == "error" {
			criticalErrors++
		}
	}
	
	for _, err := range response.SemanticErrors {
		if err.Severity == "error" {
			criticalErrors++
		}
	}
	
	return criticalErrors == 0
}

// generateSummary genera un resumen del análisis
func (ja *JSAnalyzer) generateSummary(response *AnalysisResponse) {
	var summary strings.Builder
	
	if response.Success {
		summary.WriteString("✅ Analysis completed successfully\n")
	} else {
		summary.WriteString("❌ Analysis completed with errors\n")
	}
	
	summary.WriteString(fmt.Sprintf("📊 Analysis type: %s\n", response.AnalysisType))
	summary.WriteString(fmt.Sprintf("📄 Processed %d tokens in %d lines\n", 
		response.TokenCount, response.LineCount))
	
	if len(response.LexicalErrors) > 0 {
		summary.WriteString(fmt.Sprintf("🔤 Lexical errors: %d\n", len(response.LexicalErrors)))
	}
	
	if len(response.SyntacticErrors) > 0 {
		summary.WriteString(fmt.Sprintf("🌳 Syntactic errors: %d\n", len(response.SyntacticErrors)))
	}
	
	if len(response.SemanticErrors) > 0 {
		summary.WriteString(fmt.Sprintf("🧠 Semantic issues: %d\n", len(response.SemanticErrors)))
	}
	
	if response.Statistics.Variables > 0 {
		summary.WriteString(fmt.Sprintf("📦 Variables: %d (%.1f%% used)\n", 
			response.Statistics.Variables, response.Statistics.CoveragePercent))
	}
	
	if response.Statistics.Functions > 0 {
		summary.WriteString(fmt.Sprintf("⚡ Functions: %d\n", response.Statistics.Functions))
	}
	
	if response.Statistics.MaxNesting > 0 {
		summary.WriteString(fmt.Sprintf("🔄 Max nesting: %d\n", response.Statistics.MaxNesting))
	}
	
	if response.Statistics.Complexity > 0 {
		summary.WriteString(fmt.Sprintf("🧮 Complexity: %d\n", response.Statistics.Complexity))
	}
	
	summary.WriteString(fmt.Sprintf("⏱️ Execution time: %v", response.ExecutionTime))
	
	response.Summary = summary.String()
}

// GetConfig retorna la configuración actual del analizador
func (ja *JSAnalyzer) GetConfig() AnalyzerConfig {
	return ja.config
}

// UpdateConfig actualiza la configuración del analizador
func (ja *JSAnalyzer) UpdateConfig(config AnalyzerConfig) {
	ja.config = config
}

// Version retorna la versión del analizador
func (ja *JSAnalyzer) Version() string {
	return "1.0.0"
}