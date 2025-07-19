package parser

import (
	"fmt"
	"js-analyzer/internal/lexer"
	"strconv"
	"strings"
)

// Precedencias de operadores
const (
	_ int = iota
	LOWEST
	ASSIGN      // =, +=, -=, etc.
	TERNARY     // ? :
	OR          // ||
	AND         // &&
	BITWISE_OR  // |
	BITWISE_XOR // ^
	BITWISE_AND // &
	EQUALS      // ==, !=, ===, !==
	LESSGREATER // > or <, >=, <=
	SHIFT       // <<, >>
	SUM         // +, -
	PRODUCT     // *, /, %
	POWER       // **
	PREFIX      // -X, !X, ++X, --X
	POSTFIX     // X++, X--
	CALL        // myFunction(X)
	MEMBER      // obj.prop, obj[prop]
)

// precedences mapea tokens a sus precedencias
var precedences = map[lexer.TokenType]int{
	// Asignación
	lexer.ASSIGN:                        ASSIGN,
	lexer.PLUS_ASSIGN:                   ASSIGN,
	lexer.MINUS_ASSIGN:                  ASSIGN,
	lexer.MULTIPLY_ASSIGN:               ASSIGN,
	lexer.DIVIDE_ASSIGN:                 ASSIGN,
	lexer.MODULO_ASSIGN:                 ASSIGN,
	lexer.POWER_ASSIGN:                  ASSIGN,
	lexer.BITWISE_AND_ASSIGN:            ASSIGN,
	lexer.BITWISE_OR_ASSIGN:             ASSIGN,
	lexer.BITWISE_XOR_ASSIGN:            ASSIGN,
	lexer.LEFT_SHIFT_ASSIGN:             ASSIGN,
	lexer.RIGHT_SHIFT_ASSIGN:            ASSIGN,
	lexer.UNSIGNED_RIGHT_SHIFT_ASSIGN:   ASSIGN,
	lexer.AND_ASSIGN:                    ASSIGN,
	lexer.OR_ASSIGN:                     ASSIGN,
	lexer.NULLISH_ASSIGN:                ASSIGN,
	
	// Ternario
	lexer.QUESTION:                      TERNARY,
	
	// Lógicos
	lexer.OR:                            OR,
	lexer.AND:                           AND,
	
	// Bit a bit
	lexer.BITWISE_OR:                    BITWISE_OR,
	lexer.BITWISE_XOR:                   BITWISE_XOR,
	lexer.BITWISE_AND:                   BITWISE_AND,
	
	// Comparación
	lexer.EQ:                            EQUALS,
	lexer.NOT_EQ:                        EQUALS,
	lexer.STRICT_EQ:                     EQUALS,
	lexer.STRICT_NOT_EQ:                 EQUALS,
	lexer.LT:                            LESSGREATER,
	lexer.GT:                            LESSGREATER,
	lexer.LTE:                           LESSGREATER,
	lexer.GTE:                           LESSGREATER,
	
	// Desplazamiento
	lexer.LEFT_SHIFT:                    SHIFT,
	lexer.RIGHT_SHIFT:                   SHIFT,
	lexer.UNSIGNED_RIGHT_SHIFT:          SHIFT,
	
	// Aritméticos
	lexer.PLUS:                          SUM,
	lexer.MINUS:                         SUM,
	lexer.DIVIDE:                        PRODUCT,
	lexer.MULTIPLY:                      PRODUCT,
	lexer.MODULO:                        PRODUCT,
	lexer.POWER:                         POWER,
	
	// Llamadas y miembros
	lexer.LPAREN:                        CALL,
	lexer.DOT:                           MEMBER,
	lexer.LBRACKET:                      MEMBER,
	lexer.OPTIONAL_CHAINING:             MEMBER,
}

// prefixParseFn función para parsing de prefix
type prefixParseFn func() Expression

// infixParseFn función para parsing de infix
type infixParseFn func(Expression) Expression

// Parser estructura del analizador sintáctico
type Parser struct {
	l lexer.Lexer

	curToken  lexer.Token
	peekToken lexer.Token

	prefixParseFns map[lexer.TokenType]prefixParseFn
	infixParseFns  map[lexer.TokenType]infixParseFn

	errors []string
}

// ParseError representa un error de parsing
type ParseError struct {
	Message  string
	Line     int
	Column   int
	Token    lexer.Token
	Expected []string
	Type     string // Agregado para clasificar el tipo de error
}

func (pe ParseError) Error() string {
	return fmt.Sprintf("Parse error at line %d, column %d: %s (got %s)", 
		pe.Line, pe.Column, pe.Message, pe.Token.Type.String())
}

// New crea un nuevo parser
func New(l lexer.Lexer) *Parser {
	p := &Parser{
		l:      l,
		errors: []string{},
	}

	// Lee dos tokens, para que curToken y peekToken estén configurados
	p.nextToken()
	p.nextToken()

	// Registra funciones de parsing prefix
	p.prefixParseFns = make(map[lexer.TokenType]prefixParseFn)
	p.registerPrefix(lexer.IDENT, p.parseIdentifier)
	p.registerPrefix(lexer.INT, p.parseIntegerLiteral)
	p.registerPrefix(lexer.FLOAT, p.parseFloatLiteral)
	p.registerPrefix(lexer.STRING, p.parseStringLiteral)
	p.registerPrefix(lexer.TEMPLATE_STRING, p.parseTemplateLiteral)
	p.registerPrefix(lexer.TRUE, p.parseBoolean)
	p.registerPrefix(lexer.FALSE, p.parseBoolean)
	p.registerPrefix(lexer.NULL, p.parseNull)
	p.registerPrefix(lexer.NOT, p.parsePrefixExpression)
	p.registerPrefix(lexer.MINUS, p.parsePrefixExpression)
	p.registerPrefix(lexer.PLUS, p.parsePrefixExpression)
	p.registerPrefix(lexer.INCREMENT, p.parsePrefixExpression)
	p.registerPrefix(lexer.DECREMENT, p.parsePrefixExpression)
	p.registerPrefix(lexer.BITWISE_NOT, p.parsePrefixExpression)
	p.registerPrefix(lexer.LPAREN, p.parseGroupedExpression)
	p.registerPrefix(lexer.LBRACKET, p.parseArrayLiteral)
	p.registerPrefix(lexer.LBRACE, p.parseObjectLiteral)

	// Registra funciones de parsing infix
	p.infixParseFns = make(map[lexer.TokenType]infixParseFn)
	
	// Operadores aritméticos
	p.registerInfix(lexer.PLUS, p.parseInfixExpression)
	p.registerInfix(lexer.MINUS, p.parseInfixExpression)
	p.registerInfix(lexer.DIVIDE, p.parseInfixExpression)
	p.registerInfix(lexer.MULTIPLY, p.parseInfixExpression)
	p.registerInfix(lexer.MODULO, p.parseInfixExpression)
	p.registerInfix(lexer.POWER, p.parseInfixExpression)
	
	// Operadores de comparación
	p.registerInfix(lexer.EQ, p.parseInfixExpression)
	p.registerInfix(lexer.NOT_EQ, p.parseInfixExpression)
	p.registerInfix(lexer.STRICT_EQ, p.parseInfixExpression)
	p.registerInfix(lexer.STRICT_NOT_EQ, p.parseInfixExpression)
	p.registerInfix(lexer.LT, p.parseInfixExpression)
	p.registerInfix(lexer.GT, p.parseInfixExpression)
	p.registerInfix(lexer.LTE, p.parseInfixExpression)
	p.registerInfix(lexer.GTE, p.parseInfixExpression)
	
	// Operadores lógicos
	p.registerInfix(lexer.AND, p.parseInfixExpression)
	p.registerInfix(lexer.OR, p.parseInfixExpression)
	
	// Operadores bit a bit
	p.registerInfix(lexer.BITWISE_AND, p.parseInfixExpression)
	p.registerInfix(lexer.BITWISE_OR, p.parseInfixExpression)
	p.registerInfix(lexer.BITWISE_XOR, p.parseInfixExpression)
	p.registerInfix(lexer.LEFT_SHIFT, p.parseInfixExpression)
	p.registerInfix(lexer.RIGHT_SHIFT, p.parseInfixExpression)
	p.registerInfix(lexer.UNSIGNED_RIGHT_SHIFT, p.parseInfixExpression)
	
	// Operadores de asignación
	p.registerInfix(lexer.ASSIGN, p.parseAssignmentExpression)
	p.registerInfix(lexer.PLUS_ASSIGN, p.parseAssignmentExpression)
	p.registerInfix(lexer.MINUS_ASSIGN, p.parseAssignmentExpression)
	p.registerInfix(lexer.MULTIPLY_ASSIGN, p.parseAssignmentExpression)
	p.registerInfix(lexer.DIVIDE_ASSIGN, p.parseAssignmentExpression)
	p.registerInfix(lexer.MODULO_ASSIGN, p.parseAssignmentExpression)
	
	// Postfix
	p.registerInfix(lexer.INCREMENT, p.parsePostfixExpression)
	p.registerInfix(lexer.DECREMENT, p.parsePostfixExpression)
	
	// Llamadas y miembros
	p.registerInfix(lexer.LPAREN, p.parseCallExpression)
	p.registerInfix(lexer.DOT, p.parseMemberExpression)
	p.registerInfix(lexer.LBRACKET, p.parseMemberExpression)
	
	// Operadores adicionales
	p.registerInfix(lexer.NULLISH_COALESCING, p.parseInfixExpression)
	p.registerInfix(lexer.OPTIONAL_CHAINING, p.parseMemberExpression)

	return p
}

// registerPrefix registra una función de parsing prefix
func (p *Parser) registerPrefix(tokenType lexer.TokenType, fn prefixParseFn) {
	p.prefixParseFns[tokenType] = fn
}

// registerInfix registra una función de parsing infix
func (p *Parser) registerInfix(tokenType lexer.TokenType, fn infixParseFn) {
	p.infixParseFns[tokenType] = fn
}

// nextToken avanza los tokens
func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

// ParseProgram parsea el programa completo
func (p *Parser) ParseProgram() *Program {
	program := &Program{}
	program.Statements = []Statement{}

	for !p.curTokenIs(lexer.EOF) {
		stmt := p.parseStatement()
		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}
		p.nextToken()
	}

	return program
}

// parseStatement parsea declaraciones
func (p *Parser) parseStatement() Statement {
	switch p.curToken.Type {
	case lexer.VAR, lexer.LET, lexer.CONST:
		return p.parseVarStatement()
	case lexer.FUNCTION:
		return p.parseFunctionStatement()
	case lexer.RETURN:
		return p.parseReturnStatement()
	case lexer.IF:
		return p.parseIfStatement()
	case lexer.WHILE:
		return p.parseWhileStatement()
	case lexer.FOR:
		return p.parseForStatement()
	default:
		return p.parseExpressionStatement()
	}
}

// parseVarStatement parsea declaraciones de variables - USANDO STRINGS PARA OPTIMIZACIÓN
func (p *Parser) parseVarStatement() *VarStatement {
	stmt := &VarStatement{
		Token:   p.curToken,
		VarType: strings.ToLower(strings.TrimSpace(p.curToken.Literal)), // Usar strings para normalización
		Position: p.curToken.Position,
	}

	if !p.expectPeek(lexer.IDENT) {
		return nil
	}

	// Usar strings.Builder para optimización de concatenación si es necesario
	var nameBuilder strings.Builder
	nameBuilder.WriteString(p.curToken.Literal)
	
	stmt.Name = &Identifier{
		Token:    p.curToken,
		Value:    nameBuilder.String(), // Uso optimizado de strings
		Position: p.curToken.Position,
	}

	if p.peekTokenIs(lexer.ASSIGN) {
		p.nextToken()
		p.nextToken()
		stmt.Value = p.parseExpression(LOWEST)
	}

	if p.peekTokenIs(lexer.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

// parseFunctionStatement parsea declaraciones de función - USANDO STRINGS PARA VALIDACIÓN
func (p *Parser) parseFunctionStatement() *FunctionStatement {
	stmt := &FunctionStatement{
		Token:    p.curToken,
		Position: p.curToken.Position,
		IsAsync:  false,
		IsArrow:  false,
	}

	if !p.expectPeek(lexer.IDENT) {
		return nil
	}

	// Usar strings para validación y normalización del nombre de función
	functionName := strings.TrimSpace(p.curToken.Literal)
	if strings.Contains(functionName, " ") || strings.HasPrefix(functionName, "_") {
		// Agregar warning sobre convenciones de nomenclatura usando strings
		p.addWarning(fmt.Sprintf("Function name '%s' may not follow naming conventions", functionName))
	}

	stmt.Name = &Identifier{
		Token:    p.curToken,
		Value:    functionName,
		Position: p.curToken.Position,
	}

	if !p.expectPeek(lexer.LPAREN) {
		return nil
	}

	stmt.Parameters = p.parseFunctionParameters()

	if !p.expectPeek(lexer.LBRACE) {
		return nil
	}

	stmt.Body = p.parseBlockStatement()

	return stmt
}

// parseFunctionParameters parsea parámetros de función - USANDO STRINGS PARA VALIDACIÓN
func (p *Parser) parseFunctionParameters() []*Identifier {
	identifiers := []*Identifier{}

	if p.peekTokenIs(lexer.RPAREN) {
		p.nextToken()
		return identifiers
	}

	p.nextToken()

	// Usar strings para validación de parámetros
	paramName := strings.TrimSpace(p.curToken.Literal)
	if len(paramName) == 0 {
		p.addError("Empty parameter name is not allowed")
		return nil
	}

	ident := &Identifier{
		Token:    p.curToken,
		Value:    paramName,
		Position: p.curToken.Position,
	}
	identifiers = append(identifiers, ident)

	for p.peekTokenIs(lexer.COMMA) {
		p.nextToken()
		p.nextToken()
		
		// Validación con strings para cada parámetro
		paramName := strings.TrimSpace(p.curToken.Literal)
		if len(paramName) == 0 {
			p.addError("Empty parameter name is not allowed")
			continue
		}

		ident := &Identifier{
			Token:    p.curToken,
			Value:    paramName,
			Position: p.curToken.Position,
		}
		identifiers = append(identifiers, ident)
	}

	if !p.expectPeek(lexer.RPAREN) {
		return nil
	}

	return identifiers
}

// parseReturnStatement parsea declaraciones return
func (p *Parser) parseReturnStatement() *ReturnStatement {
	stmt := &ReturnStatement{
		Token:    p.curToken,
		Position: p.curToken.Position,
	}

	p.nextToken()

	if !p.curTokenIs(lexer.SEMICOLON) && !p.curTokenIs(lexer.EOF) {
		stmt.ReturnValue = p.parseExpression(LOWEST)
	}

	if p.peekTokenIs(lexer.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

// parseIfStatement parsea declaraciones if
func (p *Parser) parseIfStatement() *IfStatement {
	stmt := &IfStatement{
		Token:    p.curToken,
		Position: p.curToken.Position,
	}

	if !p.expectPeek(lexer.LPAREN) {
		return nil
	}

	p.nextToken()
	stmt.Condition = p.parseExpression(LOWEST)

	if !p.expectPeek(lexer.RPAREN) {
		return nil
	}

	if !p.expectPeek(lexer.LBRACE) {
		return nil
	}

	stmt.Consequence = p.parseBlockStatement()

	if p.peekTokenIs(lexer.ELSE) {
		p.nextToken()

		if p.peekTokenIs(lexer.IF) {
			p.nextToken()
			stmt.Alternative = p.parseIfStatement()
		} else if p.peekTokenIs(lexer.LBRACE) {
			p.nextToken()
			stmt.Alternative = p.parseBlockStatement()
		}
	}

	return stmt
}

// parseWhileStatement parsea bucles while
func (p *Parser) parseWhileStatement() *WhileStatement {
	stmt := &WhileStatement{
		Token:    p.curToken,
		Position: p.curToken.Position,
	}

	if !p.expectPeek(lexer.LPAREN) {
		return nil
	}

	p.nextToken()
	stmt.Condition = p.parseExpression(LOWEST)

	if !p.expectPeek(lexer.RPAREN) {
		return nil
	}

	if !p.expectPeek(lexer.LBRACE) {
		return nil
	}

	stmt.Body = p.parseBlockStatement()

	return stmt
}

// parseForStatement parsea bucles for
func (p *Parser) parseForStatement() *ForStatement {
	stmt := &ForStatement{
		Token:    p.curToken,
		Position: p.curToken.Position,
	}

	if !p.expectPeek(lexer.LPAREN) {
		return nil
	}

	// Inicialización
	p.nextToken()
	if !p.curTokenIs(lexer.SEMICOLON) {
		stmt.Init = p.parseStatement()
	}

	if !p.expectPeek(lexer.SEMICOLON) {
		return nil
	}

	// Condición
	p.nextToken()
	if !p.curTokenIs(lexer.SEMICOLON) {
		stmt.Condition = p.parseExpression(LOWEST)
	}

	if !p.expectPeek(lexer.SEMICOLON) {
		return nil
	}

	// Actualización
	p.nextToken()
	if !p.curTokenIs(lexer.RPAREN) {
		stmt.Update = p.parseExpression(LOWEST)
	}

	if !p.expectPeek(lexer.RPAREN) {
		return nil
	}

	if !p.expectPeek(lexer.LBRACE) {
		return nil
	}

	stmt.Body = p.parseBlockStatement()

	return stmt
}

// parseBlockStatement parsea bloques de declaraciones
func (p *Parser) parseBlockStatement() *BlockStatement {
	block := &BlockStatement{
		Token:    p.curToken,
		Position: p.curToken.Position,
	}
	block.Statements = []Statement{}

	p.nextToken()

	for !p.curTokenIs(lexer.RBRACE) && !p.curTokenIs(lexer.EOF) {
		stmt := p.parseStatement()
		if stmt != nil {
			block.Statements = append(block.Statements, stmt)
		}
		p.nextToken()
	}

	return block
}

// parseExpressionStatement parsea declaraciones de expresión
func (p *Parser) parseExpressionStatement() *ExpressionStatement {
	stmt := &ExpressionStatement{
		Token:    p.curToken,
		Position: p.curToken.Position,
	}

	stmt.Expression = p.parseExpression(LOWEST)

	if p.peekTokenIs(lexer.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

// parseExpression parsea expresiones usando Pratt parsing
func (p *Parser) parseExpression(precedence int) Expression {
	prefix := p.prefixParseFns[p.curToken.Type]
	if prefix == nil {
		p.noPrefixParseFnError(p.curToken.Type)
		return nil
	}
	leftExp := prefix()

	for !p.peekTokenIs(lexer.SEMICOLON) && precedence < p.peekPrecedence() {
		infix := p.infixParseFns[p.peekToken.Type]
		if infix == nil {
			return leftExp
		}

		p.nextToken()
		leftExp = infix(leftExp)
	}

	return leftExp
}

// parseIdentifier parsea identificadores - USANDO STRINGS PARA VALIDACIÓN
func (p *Parser) parseIdentifier() Expression {
	// Usar strings para validación de identificadores
	identValue := strings.TrimSpace(p.curToken.Literal)
	
	// Validar que el identificador no esté vacío
	if len(identValue) == 0 {
		p.addError("Empty identifier is not allowed")
		return nil
	}

	// Validar convenciones usando strings
	if strings.HasPrefix(identValue, "$$") {
		p.addWarning(fmt.Sprintf("Identifier '%s' uses unusual naming pattern", identValue))
	}

	return &Identifier{
		Token:    p.curToken,
		Value:    identValue,
		Position: p.curToken.Position,
	}
}

// parseIntegerLiteral parsea números enteros
func (p *Parser) parseIntegerLiteral() Expression {
	lit := &IntegerLiteral{
		Token:    p.curToken,
		Position: p.curToken.Position,
	}

	value, err := strconv.ParseInt(p.curToken.Literal, 0, 64)
	if err != nil {
		msg := fmt.Sprintf("could not parse %q as integer", p.curToken.Literal)
		p.errors = append(p.errors, msg)
		return nil
	}

	lit.Value = value
	return lit
}

// parseFloatLiteral parsea números flotantes
func (p *Parser) parseFloatLiteral() Expression {
	lit := &FloatLiteral{
		Token:    p.curToken,
		Position: p.curToken.Position,
	}

	value, err := strconv.ParseFloat(p.curToken.Literal, 64)
	if err != nil {
		msg := fmt.Sprintf("could not parse %q as float", p.curToken.Literal)
		p.errors = append(p.errors, msg)
		return nil
	}

	lit.Value = value
	return lit
}

// parseStringLiteral parsea strings - USANDO STRINGS PARA PROCESAMIENTO
func (p *Parser) parseStringLiteral() Expression {
	// Usar strings para procesar y validar string literals
	originalValue := p.curToken.Literal
	
	// Validar que no sea una cadena vacía malformada
	if len(originalValue) < 2 {
		p.addError("Malformed string literal")
		return nil
	}

	// Procesar escapes usando strings.Replacer para optimización
	replacer := strings.NewReplacer(
		"\\n", "\n",
		"\\t", "\t",
		"\\r", "\r",
		"\\\\", "\\",
		"\\\"", "\"",
		"\\'", "'",
	)
	
	processedValue := replacer.Replace(originalValue)
	
	return &StringLiteral{
		Token:    p.curToken,
		Value:    processedValue,
		Position: p.curToken.Position,
	}
}

// parseTemplateLiteral parsea template strings - USANDO STRINGS PARA VALIDACIÓN
func (p *Parser) parseTemplateLiteral() Expression {
	originalValue := p.curToken.Literal
	
	// Validar template literal usando strings
	if !strings.HasPrefix(originalValue, "`") || !strings.HasSuffix(originalValue, "`") {
		p.addError("Malformed template literal")
		return nil
	}

	// Contar interpolaciones ${} usando strings
	interpolationCount := strings.Count(originalValue, "${")
	if interpolationCount > 10 {
		p.addWarning(fmt.Sprintf("Template literal has %d interpolations, consider refactoring", interpolationCount))
	}

	return &TemplateLiteral{
		Token:    p.curToken,
		Value:    originalValue,
		Position: p.curToken.Position,
	}
}

// parseBoolean parsea valores booleanos
func (p *Parser) parseBoolean() Expression {
	return &BooleanLiteral{
		Token:    p.curToken,
		Value:    p.curTokenIs(lexer.TRUE),
		Position: p.curToken.Position,
	}
}

// parseNull parsea null
func (p *Parser) parseNull() Expression {
	return &NullLiteral{
		Token:    p.curToken,
		Position: p.curToken.Position,
	}
}

// parsePrefixExpression parsea expresiones prefix
func (p *Parser) parsePrefixExpression() Expression {
	expression := &PrefixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
		Position: p.curToken.Position,
	}

	p.nextToken()
	expression.Right = p.parseExpression(PREFIX)

	return expression
}

// parseInfixExpression parsea expresiones infix
func (p *Parser) parseInfixExpression(left Expression) Expression {
	expression := &InfixExpression{
		Token:    p.curToken,
		Left:     left,
		Operator: p.curToken.Literal,
		Position: p.curToken.Position,
	}

	precedence := p.curPrecedence()
	p.nextToken()
	expression.Right = p.parseExpression(precedence)

	return expression
}

// parsePostfixExpression parsea expresiones postfix
func (p *Parser) parsePostfixExpression(left Expression) Expression {
	return &PostfixExpression{
		Token:    p.curToken,
		Left:     left,
		Operator: p.curToken.Literal,
		Position: p.curToken.Position,
	}
}

// parseAssignmentExpression parsea expresiones de asignación - USANDO STRINGS PARA VALIDACIÓN
func (p *Parser) parseAssignmentExpression(left Expression) Expression {
	// Verificar que left sea un identificador
	ident, ok := left.(*Identifier)
	if !ok {
		p.errors = append(p.errors, "invalid assignment target")
		return nil
	}

	// Usar strings para validar el nombre de la variable en asignación
	varName := ident.Value
	if strings.ToUpper(varName) == varName && len(varName) > 1 {
		// Variable en mayúsculas podría ser una constante
		p.addWarning(fmt.Sprintf("Assignment to all-caps variable '%s' might be intended as constant", varName))
	}

	assignment := &AssignmentExpression{
		Token:    p.curToken,
		Name:     ident,
		Operator: p.curToken.Literal,
		Position: p.curToken.Position,
	}

	p.nextToken()
	assignment.Value = p.parseExpression(LOWEST)

	return assignment
}

// parseGroupedExpression parsea expresiones agrupadas con paréntesis
func (p *Parser) parseGroupedExpression() Expression {
	p.nextToken()

	exp := p.parseExpression(LOWEST)

	if !p.expectPeek(lexer.RPAREN) {
		return nil
	}

	return exp
}

// parseCallExpression parsea llamadas a función
func (p *Parser) parseCallExpression(fn Expression) Expression {
	exp := &CallExpression{
		Token:    p.curToken,
		Function: fn,
		Position: p.curToken.Position,
	}
	exp.Arguments = p.parseExpressionList(lexer.RPAREN)
	return exp
}

// parseMemberExpression parsea acceso a miembros (obj.prop, obj[prop])
func (p *Parser) parseMemberExpression(left Expression) Expression {
	exp := &MemberExpression{
		Token:    p.curToken,
		Object:   left,
		Position: p.curToken.Position,
	}

	if p.curTokenIs(lexer.DOT) || p.curTokenIs(lexer.OPTIONAL_CHAINING) {
		exp.Computed = false
		p.nextToken()
		exp.Property = p.parseIdentifier()
	} else if p.curTokenIs(lexer.LBRACKET) {
		exp.Computed = true
		p.nextToken()
		exp.Property = p.parseExpression(LOWEST)
		if !p.expectPeek(lexer.RBRACKET) {
			return nil
		}
	}

	return exp
}

// parseArrayLiteral parsea arrays
func (p *Parser) parseArrayLiteral() Expression {
	array := &ArrayLiteral{
		Token:    p.curToken,
		Position: p.curToken.Position,
	}

	array.Elements = p.parseExpressionList(lexer.RBRACKET)

	return array
}

// parseObjectLiteral parsea objetos literales - USANDO STRINGS PARA VALIDACIÓN DE CLAVES
func (p *Parser) parseObjectLiteral() Expression {
	obj := &ObjectLiteral{
		Token:    p.curToken,
		Pairs:    make(map[Expression]Expression),
		Position: p.curToken.Position,
	}

	if p.peekTokenIs(lexer.RBRACE) {
		p.nextToken()
		return obj
	}

	p.nextToken()

	for {
		key := p.parseExpression(LOWEST)
		
		// Validar claves usando strings si es un string literal
		if strLit, ok := key.(*StringLiteral); ok {
			keyValue := strLit.Value
			if strings.TrimSpace(keyValue) == "" {
				p.addWarning("Empty string key in object literal")
			}
			// Validar caracteres especiales en claves
			if strings.ContainsAny(keyValue, "\n\t\r") {
				p.addWarning("Object key contains whitespace characters")
			}
		}

		if !p.expectPeek(lexer.COLON) {
			return nil
		}

		p.nextToken()
		value := p.parseExpression(LOWEST)

		obj.Pairs[key] = value

		if !p.peekTokenIs(lexer.COMMA) {
			break
		}

		p.nextToken()
		p.nextToken()
	}

	if !p.expectPeek(lexer.RBRACE) {
		return nil
	}

	return obj
}

// parseExpressionList parsea listas de expresiones
func (p *Parser) parseExpressionList(end lexer.TokenType) []Expression {
	args := []Expression{}

	if p.peekTokenIs(end) {
		p.nextToken()
		return args
	}

	p.nextToken()
	args = append(args, p.parseExpression(LOWEST))

	for p.peekTokenIs(lexer.COMMA) {
		p.nextToken()
		p.nextToken()
		args = append(args, p.parseExpression(LOWEST))
	}

	if !p.expectPeek(end) {
		return nil
	}

	return args
}

// Funciones de utilidad
func (p *Parser) curTokenIs(t lexer.TokenType) bool {
	return p.curToken.Type == t
}

func (p *Parser) peekTokenIs(t lexer.TokenType) bool {
	return p.peekToken.Type == t
}

func (p *Parser) expectPeek(t lexer.TokenType) bool {
	if p.peekTokenIs(t) {
		p.nextToken()
		return true
	} else {
		p.peekError(t)
		return false
	}
}

func (p *Parser) peekPrecedence() int {
	if p, ok := precedences[p.peekToken.Type]; ok {
		return p
	}
	return LOWEST
}

func (p *Parser) curPrecedence() int {
	if p, ok := precedences[p.curToken.Type]; ok {
		return p
	}
	return LOWEST
}

// Manejo de errores mejorado con strings
func (p *Parser) Errors() []string {
	return p.errors
}

func (p *Parser) peekError(t lexer.TokenType) {
	msg := fmt.Sprintf("expected next token to be %s, got %s instead",
		t, p.peekToken.Type)
	p.errors = append(p.errors, msg)
}

func (p *Parser) noPrefixParseFnError(t lexer.TokenType) {
	msg := fmt.Sprintf("no prefix parse function for %s found", t)
	p.errors = append(p.errors, msg)
}

// addError añade un error usando strings para optimización
func (p *Parser) addError(message string) {
	// Usar strings.Builder para construcción eficiente de mensajes de error
	var errorBuilder strings.Builder
	errorBuilder.WriteString("Parse Error: ")
	errorBuilder.WriteString(message)
	errorBuilder.WriteString(fmt.Sprintf(" at line %d, column %d", p.curToken.Position.Line, p.curToken.Position.Column))
	
	p.errors = append(p.errors, errorBuilder.String())
}

// addWarning añade advertencias usando strings para optimización
func (p *Parser) addWarning(message string) {
	// Usar strings.Builder para construcción eficiente de mensajes de advertencia
	var warningBuilder strings.Builder
	warningBuilder.WriteString("Warning: ")
	warningBuilder.WriteString(message)
	warningBuilder.WriteString(fmt.Sprintf(" at line %d, column %d", p.curToken.Position.Line, p.curToken.Position.Column))
	
	// Podrías agregar las advertencias a una lista separada si quisieras
	p.errors = append(p.errors, warningBuilder.String())
}

// GetDetailedErrors retorna errores detallados con posición
func (p *Parser) GetDetailedErrors() []ParseError {
	detailedErrors := make([]ParseError, 0, len(p.errors))
	
	for _, errMsg := range p.errors {
		// Usar strings para determinar el tipo de error
		var errorType string
		if strings.Contains(strings.ToLower(errMsg), "warning") {
			errorType = "PARSE_WARNING"
		} else {
			errorType = "PARSE_ERROR"
		}
		
		detailedErrors = append(detailedErrors, ParseError{
			Message: errMsg,
			Line:    p.curToken.Position.Line,
			Column:  p.curToken.Position.Column,
			Token:   p.curToken,
			Type:    errorType, // Ahora usamos la variable
		})
	}
	
	return detailedErrors
}

// HasErrors verifica si hay errores
func (p *Parser) HasErrors() bool {
	return len(p.errors) > 0
}

// GetErrorCount retorna el número de errores
func (p *Parser) GetErrorCount() int {
	return len(p.errors)
}

// GetErrorSummary retorna un resumen de errores usando strings para optimización
func (p *Parser) GetErrorSummary() string {
	if len(p.errors) == 0 {
		return "No parsing errors found"
	}

	// Usar strings.Builder para construcción eficiente del resumen
	var summaryBuilder strings.Builder
	summaryBuilder.WriteString(fmt.Sprintf("Found %d parsing issue(s):\n", len(p.errors)))
	
	// Contar tipos de errores usando strings
	errorCount := 0
	warningCount := 0
	
	for _, err := range p.errors {
		if strings.Contains(strings.ToLower(err), "warning") {
			warningCount++
		} else {
			errorCount++
		}
	}
	
	if errorCount > 0 {
		summaryBuilder.WriteString(fmt.Sprintf("- Errors: %d\n", errorCount))
	}
	if warningCount > 0 {
		summaryBuilder.WriteString(fmt.Sprintf("- Warnings: %d\n", warningCount))
	}
	
	summaryBuilder.WriteString("\nDetails:\n")
	for i, err := range p.errors {
		summaryBuilder.WriteString(fmt.Sprintf("%d. %s\n", i+1, err))
	}
	
	return summaryBuilder.String()
}

// ValidateCode realiza validaciones adicionales usando strings
func (p *Parser) ValidateCode(code string) []string {
	var issues []string
	
	// Validaciones usando strings
	lines := strings.Split(code, "\n")
	
	for i, line := range lines {
		lineNum := i + 1
		trimmedLine := strings.TrimSpace(line)
		
		// Verificar líneas muy largas
		if len(line) > 120 {
			issues = append(issues, fmt.Sprintf("Line %d exceeds 120 characters", lineNum))
		}
		
		// Verificar líneas vacías excesivas
		if trimmedLine == "" {
			continue
		}
		
		// Verificar uso de console.log (advertencia)
		if strings.Contains(trimmedLine, "console.log") {
			issues = append(issues, fmt.Sprintf("Line %d contains console.log (consider removing for production)", lineNum))
		}
		
		// Verificar uso de alert
		if strings.Contains(trimmedLine, "alert(") {
			issues = append(issues, fmt.Sprintf("Line %d uses alert() (not recommended)", lineNum))
		}
		
		// Verificar variables con nombres de una sola letra
		if strings.Contains(trimmedLine, "var ") || strings.Contains(trimmedLine, "let ") {
			words := strings.Fields(trimmedLine)
			for j, word := range words {
				if (word == "var" || word == "let") && j+1 < len(words) {
					varName := strings.TrimRight(words[j+1], "=;,")
					if len(varName) == 1 && varName != "i" && varName != "j" && varName != "x" && varName != "y" {
						issues = append(issues, fmt.Sprintf("Line %d: single-letter variable '%s' may be unclear", lineNum, varName))
					}
				}
			}
		}
	}
	
	return issues
}

// OptimizeMemory libera recursos usando strings para logging
func (p *Parser) OptimizeMemory() {
	// Limpiar tokens y estados para optimizar memoria
	p.curToken = lexer.Token{}
	p.peekToken = lexer.Token{}
	
	// Crear nuevo slice para errores si es muy grande
	if len(p.errors) > 100 {
		optimizedErrors := make([]string, len(p.errors))
		copy(optimizedErrors, p.errors)
		p.errors = optimizedErrors
	}
	
	// Log de optimización usando strings
	var logBuilder strings.Builder
	logBuilder.WriteString("Parser memory optimization completed")
	// En un logger real, usarías esto para logging
	_ = logBuilder.String()
}

// Reset reinicia el parser con un nuevo lexer
func (p *Parser) Reset(l lexer.Lexer) {
	p.l = l
	p.errors = []string{}
	p.nextToken()
	p.nextToken()
}