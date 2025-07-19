package lexer

import (
	"js-analyzer/internal/lexer/core"
	"js-analyzer/internal/lexer/types"
)

// Lexer interfaz pública del analizador léxico
type Lexer interface {
	NextToken() types.Token
	GetAllTokens() []types.Token
	GetPosition() types.Position
	Reset(input string)
}

// New crea una nueva instancia del lexer
func New(input string) Lexer {
	return core.NewLexer(input)
}

// TokenType re-exporta el tipo de token para compatibilidad
type TokenType = types.TokenType

// Token re-exporta el tipo de token para compatibilidad
type Token = types.Token

// Position re-exporta el tipo de posición para compatibilidad
type Position = types.Position

// Re-exportar constantes de tokens para compatibilidad
const (
	ILLEGAL = types.ILLEGAL
	EOF     = types.EOF
	
	// Identificadores y literales
	IDENT   = types.IDENT
	INT     = types.INT
	FLOAT   = types.FLOAT
	STRING  = types.STRING
	BOOLEAN = types.BOOLEAN
	
	// Operadores
	ASSIGN   = types.ASSIGN
	PLUS     = types.PLUS
	MINUS    = types.MINUS
	MULTIPLY = types.MULTIPLY
	DIVIDE   = types.DIVIDE
	MODULO   = types.MODULO
	
	// Operadores de comparación
	EQ     = types.EQ
	NOT_EQ = types.NOT_EQ
	LT     = types.LT
	GT     = types.GT
	LTE    = types.LTE
	GTE    = types.GTE
	
	// Operadores lógicos
	AND = types.AND
	OR  = types.OR
	NOT = types.NOT
	
	// Operadores de asignación básicos
	PLUS_ASSIGN     = types.PLUS_ASSIGN
	MINUS_ASSIGN    = types.MINUS_ASSIGN
	MULTIPLY_ASSIGN = types.MULTIPLY_ASSIGN
	DIVIDE_ASSIGN   = types.DIVIDE_ASSIGN
	MODULO_ASSIGN   = types.MODULO_ASSIGN
	POWER_ASSIGN    = types.POWER_ASSIGN
	
	// Operadores de asignación bit a bit
	BITWISE_AND_ASSIGN          = types.BITWISE_AND_ASSIGN
	BITWISE_OR_ASSIGN           = types.BITWISE_OR_ASSIGN
	BITWISE_XOR_ASSIGN          = types.BITWISE_XOR_ASSIGN
	LEFT_SHIFT_ASSIGN           = types.LEFT_SHIFT_ASSIGN
	RIGHT_SHIFT_ASSIGN          = types.RIGHT_SHIFT_ASSIGN
	UNSIGNED_RIGHT_SHIFT_ASSIGN = types.UNSIGNED_RIGHT_SHIFT_ASSIGN
	
	// Operadores de asignación lógicos
	AND_ASSIGN     = types.AND_ASSIGN
	OR_ASSIGN      = types.OR_ASSIGN
	NULLISH_ASSIGN = types.NULLISH_ASSIGN
	
	// Operadores de incremento/decremento
	INCREMENT = types.INCREMENT
	DECREMENT = types.DECREMENT
	
	// Operadores de comparación estricta
	STRICT_EQ     = types.STRICT_EQ
	STRICT_NOT_EQ = types.STRICT_NOT_EQ
	
	// Operadores bit a bit
	BITWISE_AND          = types.BITWISE_AND
	BITWISE_OR           = types.BITWISE_OR
	BITWISE_XOR          = types.BITWISE_XOR
	BITWISE_NOT          = types.BITWISE_NOT
	LEFT_SHIFT           = types.LEFT_SHIFT
	RIGHT_SHIFT          = types.RIGHT_SHIFT
	UNSIGNED_RIGHT_SHIFT = types.UNSIGNED_RIGHT_SHIFT
	
	// Operadores adicionales
	POWER               = types.POWER
	NULLISH_COALESCING  = types.NULLISH_COALESCING
	OPTIONAL_CHAINING   = types.OPTIONAL_CHAINING
	ARROW               = types.ARROW
	SPREAD              = types.SPREAD
	
	// Delimitadores adicionales
	DOT      = types.DOT
	QUESTION = types.QUESTION
	
	// Tipos de string
	TEMPLATE_STRING = types.TEMPLATE_STRING
	
	// Comentarios
	COMMENT_LINE  = types.COMMENT_LINE
	COMMENT_BLOCK = types.COMMENT_BLOCK
	
	// Delimitadores
	COMMA     = types.COMMA
	SEMICOLON = types.SEMICOLON
	COLON     = types.COLON
	
	// Paréntesis y llaves
	LPAREN   = types.LPAREN
	RPAREN   = types.RPAREN
	LBRACE   = types.LBRACE
	RBRACE   = types.RBRACE
	LBRACKET = types.LBRACKET
	RBRACKET = types.RBRACKET
	
	// Palabras clave
	FUNCTION  = types.FUNCTION
	VAR       = types.VAR
	LET       = types.LET
	CONST     = types.CONST
	IF        = types.IF
	ELSE      = types.ELSE
	FOR       = types.FOR
	WHILE     = types.WHILE
	RETURN    = types.RETURN
	TRUE      = types.TRUE
	FALSE     = types.FALSE
	NULL      = types.NULL
	UNDEFINED = types.UNDEFINED
)

// LookupIdent re-exporta la función para compatibilidad
var LookupIdent = types.LookupIdent