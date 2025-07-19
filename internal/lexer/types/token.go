package types

import "fmt"

// TokenType representa el tipo de token
type TokenType int

const (
	// Tokens especiales
	ILLEGAL TokenType = iota
	EOF
	
	// Identificadores y literales
	IDENT   // variables, funciones
	INT     // 123456
	FLOAT   // 123.45
	STRING  // "hello"
	BOOLEAN // true, false
	
	// Operadores básicos
	ASSIGN   // =
	PLUS     // +
	MINUS    // -
	MULTIPLY // *
	DIVIDE   // /
	MODULO   // %
	
	// Operadores de comparación
	EQ     // ==
	NOT_EQ // !=
	LT     // <
	GT     // >
	LTE    // <=
	GTE    // >=
	
	// Operadores de comparación estricta
	STRICT_EQ     // ===
	STRICT_NOT_EQ // !==
	
	// Operadores lógicos
	AND // &&
	OR  // ||
	NOT // !
	
	// Operadores de asignación básicos
	PLUS_ASSIGN     // +=
	MINUS_ASSIGN    // -=
	MULTIPLY_ASSIGN // *=
	DIVIDE_ASSIGN   // /=
	MODULO_ASSIGN   // %=
	POWER_ASSIGN    // **=
	
	// Operadores de asignación bit a bit
	BITWISE_AND_ASSIGN          // &=
	BITWISE_OR_ASSIGN           // |=
	BITWISE_XOR_ASSIGN          // ^=
	LEFT_SHIFT_ASSIGN           // <<=
	RIGHT_SHIFT_ASSIGN          // >>=
	UNSIGNED_RIGHT_SHIFT_ASSIGN // >>>=
	
	// Operadores de asignación lógicos
	AND_ASSIGN     // &&=
	OR_ASSIGN      // ||=
	NULLISH_ASSIGN // ??=
	
	// Operadores de incremento/decremento
	INCREMENT // ++
	DECREMENT // --
	
	// Operadores bit a bit
	BITWISE_AND          // &
	BITWISE_OR           // |
	BITWISE_XOR          // ^
	BITWISE_NOT          // ~
	LEFT_SHIFT           // <<
	RIGHT_SHIFT          // >>
	UNSIGNED_RIGHT_SHIFT // >>>
	
	// Operadores adicionales
	POWER               // **
	NULLISH_COALESCING  // ??
	OPTIONAL_CHAINING   // ?.
	ARROW               // =>
	SPREAD              // ...
	
	// Delimitadores
	COMMA     // ,
	SEMICOLON // ;
	COLON     // :
	DOT       // .
	QUESTION  // ?
	
	// Paréntesis y llaves
	LPAREN   // (
	RPAREN   // )
	LBRACE   // {
	RBRACE   // }
	LBRACKET // [
	RBRACKET // ]
	
	// Tipos de string
	TEMPLATE_STRING // `template`
	
	// Comentarios (opcionales)
	COMMENT_LINE  // //
	COMMENT_BLOCK // /* */
	
	// Palabras clave
	FUNCTION  // function
	VAR       // var
	LET       // let
	CONST     // const
	IF        // if
	ELSE      // else
	FOR       // for
	WHILE     // while
	RETURN    // return
	TRUE      // true
	FALSE     // false
	NULL      // null
	UNDEFINED // undefined
)

// Token representa un token individual
type Token struct {
	Type     TokenType `json:"type"`
	Literal  string    `json:"literal"`
	Position Position  `json:"position"`
}

// String retorna la representación en string del token
func (t Token) String() string {
	return fmt.Sprintf("Token{Type: %s, Literal: %q, Position: %s}", 
		t.Type.String(), t.Literal, t.Position.String())
}

// IsValid verifica si el token es válido
func (t Token) IsValid() bool {
	return t.Type != ILLEGAL
}

// IsEOF verifica si el token es EOF
func (t Token) IsEOF() bool {
	return t.Type == EOF
}

// IsKeyword verifica si el token es una palabra clave
func (t Token) IsKeyword() bool {
	return t.Type >= FUNCTION && t.Type <= UNDEFINED
}

// IsOperator verifica si el token es un operador
func (t Token) IsOperator() bool {
	return (t.Type >= ASSIGN && t.Type <= MODULO) || 
		   (t.Type >= EQ && t.Type <= NOT) ||
		   (t.Type >= PLUS_ASSIGN && t.Type <= SPREAD)
}

// IsLiteral verifica si el token es un literal
func (t Token) IsLiteral() bool {
	return t.Type == INT || t.Type == FLOAT || t.Type == STRING || 
		   t.Type == TRUE || t.Type == FALSE || t.Type == TEMPLATE_STRING
}

// IsDelimiter verifica si el token es un delimitador
func (t Token) IsDelimiter() bool {
	return t.Type >= COMMA && t.Type <= RBRACKET
}

// String retorna el nombre del tipo de token
func (tt TokenType) String() string {
	switch tt {
	case ILLEGAL:
		return "ILLEGAL"
	case EOF:
		return "EOF"
	case IDENT:
		return "IDENT"
	case INT:
		return "INT"
	case FLOAT:
		return "FLOAT"
	case STRING:
		return "STRING"
	case BOOLEAN:
		return "BOOLEAN"
	case ASSIGN:
		return "ASSIGN"
	case PLUS:
		return "PLUS"
	case MINUS:
		return "MINUS"
	case MULTIPLY:
		return "MULTIPLY"
	case DIVIDE:
		return "DIVIDE"
	case MODULO:
		return "MODULO"
	case EQ:
		return "EQ"
	case NOT_EQ:
		return "NOT_EQ"
	case LT:
		return "LT"
	case GT:
		return "GT"
	case LTE:
		return "LTE"
	case GTE:
		return "GTE"
	case STRICT_EQ:
		return "STRICT_EQ"
	case STRICT_NOT_EQ:
		return "STRICT_NOT_EQ"
	case AND:
		return "AND"
	case OR:
		return "OR"
	case NOT:
		return "NOT"
	case PLUS_ASSIGN:
		return "PLUS_ASSIGN"
	case MINUS_ASSIGN:
		return "MINUS_ASSIGN"
	case MULTIPLY_ASSIGN:
		return "MULTIPLY_ASSIGN"
	case DIVIDE_ASSIGN:
		return "DIVIDE_ASSIGN"
	case MODULO_ASSIGN:
		return "MODULO_ASSIGN"
	case POWER_ASSIGN:
		return "POWER_ASSIGN"
	case BITWISE_AND_ASSIGN:
		return "BITWISE_AND_ASSIGN"
	case BITWISE_OR_ASSIGN:
		return "BITWISE_OR_ASSIGN"
	case BITWISE_XOR_ASSIGN:
		return "BITWISE_XOR_ASSIGN"
	case LEFT_SHIFT_ASSIGN:
		return "LEFT_SHIFT_ASSIGN"
	case RIGHT_SHIFT_ASSIGN:
		return "RIGHT_SHIFT_ASSIGN"
	case UNSIGNED_RIGHT_SHIFT_ASSIGN:
		return "UNSIGNED_RIGHT_SHIFT_ASSIGN"
	case AND_ASSIGN:
		return "AND_ASSIGN"
	case OR_ASSIGN:
		return "OR_ASSIGN"
	case NULLISH_ASSIGN:
		return "NULLISH_ASSIGN"
	case INCREMENT:
		return "INCREMENT"
	case DECREMENT:
		return "DECREMENT"
	case BITWISE_AND:
		return "BITWISE_AND"
	case BITWISE_OR:
		return "BITWISE_OR"
	case BITWISE_XOR:
		return "BITWISE_XOR"
	case BITWISE_NOT:
		return "BITWISE_NOT"
	case LEFT_SHIFT:
		return "LEFT_SHIFT"
	case RIGHT_SHIFT:
		return "RIGHT_SHIFT"
	case UNSIGNED_RIGHT_SHIFT:
		return "UNSIGNED_RIGHT_SHIFT"
	case POWER:
		return "POWER"
	case NULLISH_COALESCING:
		return "NULLISH_COALESCING"
	case OPTIONAL_CHAINING:
		return "OPTIONAL_CHAINING"
	case ARROW:
		return "ARROW"
	case SPREAD:
		return "SPREAD"
	case COMMA:
		return "COMMA"
	case SEMICOLON:
		return "SEMICOLON"
	case COLON:
		return "COLON"
	case DOT:
		return "DOT"
	case QUESTION:
		return "QUESTION"
	case LPAREN:
		return "LPAREN"
	case RPAREN:
		return "RPAREN"
	case LBRACE:
		return "LBRACE"
	case RBRACE:
		return "RBRACE"
	case LBRACKET:
		return "LBRACKET"
	case RBRACKET:
		return "RBRACKET"
	case TEMPLATE_STRING:
		return "TEMPLATE_STRING"
	case COMMENT_LINE:
		return "COMMENT_LINE"
	case COMMENT_BLOCK:
		return "COMMENT_BLOCK"
	case FUNCTION:
		return "FUNCTION"
	case VAR:
		return "VAR"
	case LET:
		return "LET"
	case CONST:
		return "CONST"
	case IF:
		return "IF"
	case ELSE:
		return "ELSE"
	case FOR:
		return "FOR"
	case WHILE:
		return "WHILE"
	case RETURN:
		return "RETURN"
	case TRUE:
		return "TRUE"
	case FALSE:
		return "FALSE"
	case NULL:
		return "NULL"
	case UNDEFINED:
		return "UNDEFINED"
	default:
		return "UNKNOWN"
	}
}

// keywords es un mapa de palabras clave
var keywords = map[string]TokenType{
	"function":  FUNCTION,
	"var":       VAR,
	"let":       LET,
	"const":     CONST,
	"if":        IF,
	"else":      ELSE,
	"for":       FOR,
	"while":     WHILE,
	"return":    RETURN,
	"true":      TRUE,
	"false":     FALSE,
	"null":      NULL,
	"undefined": UNDEFINED,
}

// LookupIdent verifica si un identificador es una palabra clave
func LookupIdent(ident string) TokenType {
	if tok, ok := keywords[ident]; ok {
		return tok
	}
	return IDENT
}

// GetKeywords retorna todas las palabras clave
func GetKeywords() map[string]TokenType {
	result := make(map[string]TokenType)
	for k, v := range keywords {
		result[k] = v
	}
	return result
}

// IsKeyword verifica si una string es palabra clave
func IsKeyword(word string) bool {
	_, ok := keywords[word]
	return ok
}

// GetTokenCategory retorna la categoría de un token
func (tt TokenType) GetCategory() string {
	switch {
	case tt == ILLEGAL || tt == EOF:
		return "Special"
	case tt == IDENT:
		return "Identifier"
	case tt >= INT && tt <= BOOLEAN:
		return "Literal"
	case tt >= ASSIGN && tt <= SPREAD:
		return "Operator"
	case tt >= COMMA && tt <= RBRACKET:
		return "Delimiter"
	case tt == TEMPLATE_STRING:
		return "Template"
	case tt >= COMMENT_LINE && tt <= COMMENT_BLOCK:
		return "Comment"
	case tt >= FUNCTION && tt <= UNDEFINED:
		return "Keyword"
	default:
		return "Unknown"
	}
}

// IsAssignmentOperator verifica si es un operador de asignación
func (tt TokenType) IsAssignmentOperator() bool {
	return tt == ASSIGN || 
		   (tt >= PLUS_ASSIGN && tt <= POWER_ASSIGN) ||
		   (tt >= BITWISE_AND_ASSIGN && tt <= NULLISH_ASSIGN)
}

// IsComparisonOperator verifica si es un operador de comparación
func (tt TokenType) IsComparisonOperator() bool {
	return (tt >= EQ && tt <= GTE) || (tt >= STRICT_EQ && tt <= STRICT_NOT_EQ)
}

// IsArithmeticOperator verifica si es un operador aritmético
func (tt TokenType) IsArithmeticOperator() bool {
	return (tt >= PLUS && tt <= MODULO) || tt == POWER || 
		   tt == INCREMENT || tt == DECREMENT
}