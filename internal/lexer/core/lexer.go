package core

import (
	"js-analyzer/internal/lexer/types"
	"js-analyzer/internal/lexer/utils"
)

// Lexer estructura del analizador léxico optimizado
type Lexer struct {
	input        string
	position     int  // posición actual en input (apunta al carácter actual)
	readPosition int  // posición de lectura actual en input (después del carácter actual)
	ch           byte // carácter actual bajo examinación
	line         int  // línea actual
	column       int  // columna actual
	
	// Estadísticas y configuración
	tokenCount   int
	config       LexerConfig
}

// LexerConfig configuración del lexer
type LexerConfig struct {
	SkipComments    bool // Si saltar comentarios o incluirlos como tokens
	SkipWhitespace  bool // Si saltar espacios en blanco
	MaxTokenLength  int  // Longitud máxima de un token
	EnableUnicode   bool // Soporte para Unicode
	StrictMode      bool // Modo estricto de parsing
}

// DefaultConfig configuración por defecto
var DefaultConfig = LexerConfig{
	SkipComments:   true,
	SkipWhitespace: true,
	MaxTokenLength: 1000,
	EnableUnicode:  false,
	StrictMode:     false,
}

// NewLexer crea una nueva instancia del lexer con optimizaciones
func NewLexer(input string) *Lexer {
	return NewLexerWithConfig(input, DefaultConfig)
}

// NewLexerWithConfig crea una nueva instancia del lexer con configuración personalizada
func NewLexerWithConfig(input string, config LexerConfig) *Lexer {
	l := &Lexer{
		input:      input,
		line:       1,
		column:     0,
		tokenCount: 0,
		config:     config,
	}
	l.readChar()
	return l
}

// readChar lee el siguiente carácter y avanza la posición en el input
func (l *Lexer) readChar() {
	if l.readPosition >= len(l.input) {
		l.ch = 0 // ASCII NUL representa "EOF"
	} else {
		l.ch = l.input[l.readPosition]
	}
	l.position = l.readPosition
	l.readPosition++
	
	if l.ch == '\n' {
		l.line++
		l.column = 0
	} else {
		l.column++
	}
}

// peekChar retorna el siguiente carácter sin avanzar la posición
func (l *Lexer) peekChar() byte {
	if l.readPosition >= len(l.input) {
		return 0
	}
	return l.input[l.readPosition]
}

// peekCharN retorna el carácter n posiciones adelante sin avanzar
func (l *Lexer) peekCharN(n int) byte {
	pos := l.readPosition + n - 1
	if pos >= len(l.input) {
		return 0
	}
	return l.input[pos]
}

// getCurrentPosition retorna la posición actual
func (l *Lexer) getCurrentPosition() types.Position {
	return types.NewPosition(l.line, l.column, l.position)
}

// skipWhitespace salta espacios en blanco optimizado
func (l *Lexer) skipWhitespace() {
	for utils.IsWhitespace(l.ch) {
		l.readChar()
	}
}

// skipComments salta comentarios de línea y bloque
func (l *Lexer) skipComments() {
	if l.ch == '/' && l.peekChar() == '/' {
		// Comentario de línea
		for l.ch != '\n' && l.ch != 0 {
			l.readChar()
		}
	} else if l.ch == '/' && l.peekChar() == '*' {
		// Comentario de bloque
		l.readChar() // salta '/'
		l.readChar() // salta '*'
		for {
			if l.ch == '*' && l.peekChar() == '/' {
				l.readChar() // salta '*'
				l.readChar() // salta '/'
				break
			}
			if l.ch == 0 {
				break
			}
			l.readChar()
		}
	}
}

// readIdentifier lee identificadores optimizado
func (l *Lexer) readIdentifier() string {
	builder := utils.NewIdentifierBuilder()
	defer func() {
		if builder != nil {
			// El builder se limpia automáticamente en String()
		}
	}()
	
	for utils.IsValidIdentifierPart(l.ch) {
		builder.WriteByte(l.ch)
		l.readChar()
		
		// Verificar longitud máxima
		if l.config.MaxTokenLength > 0 && builder.Len() > l.config.MaxTokenLength {
			break
		}
	}
	
	return builder.String()
}

// readNumber lee números enteros y flotantes
func (l *Lexer) readNumber() (string, types.TokenType) {
	builder := utils.NewNumberBuilder()
	
	// Leer parte entera
	for utils.IsDigit(l.ch) {
		builder.WriteByte(l.ch)
		l.readChar()
		
		if l.config.MaxTokenLength > 0 && builder.Len() > l.config.MaxTokenLength {
			break
		}
	}
	
	// Verificar si es un float
	if l.ch == '.' && utils.IsDigit(l.peekChar()) {
		builder.WriteDecimalPoint()
		l.readChar()
		
		for utils.IsDigit(l.ch) {
			builder.WriteByte(l.ch)
			l.readChar()
			
			if l.config.MaxTokenLength > 0 && builder.Len() > l.config.MaxTokenLength {
				break
			}
		}
	}
	
	// Verificar notación científica (e, E)
	if l.ch == 'e' || l.ch == 'E' {
		builder.WriteByte(l.ch)
		l.readChar()
		
		// Signo opcional
		if l.ch == '+' || l.ch == '-' {
			builder.WriteByte(l.ch)
			l.readChar()
		}
		
		// Exponente
		for utils.IsDigit(l.ch) {
			builder.WriteByte(l.ch)
			l.readChar()
		}
		
		return builder.String(), types.FLOAT
	}
	
	tokenType := types.INT
	if builder.IsFloat() {
		tokenType = types.FLOAT
	}
	
	return builder.String(), tokenType
}

// readString lee strings con escape characters
func (l *Lexer) readString() string {
	quote := l.ch
	builder := utils.NewStringLiteralBuilder(quote)
	l.readChar() // salta la comilla inicial
	
	for l.ch != quote && l.ch != 0 {
		if l.ch == '\\' {
			l.readChar()
			if utils.IsEscapeChar(l.ch) {
				builder.WriteEscape(l.ch)
			} else {
				// Carácter no reconocido como escape, incluir literalmente
				builder.WriteByte('\\')
				builder.WriteByte(l.ch)
			}
		} else {
			builder.WriteByte(l.ch)
		}
		l.readChar()
		
		// Verificar longitud máxima
		if l.config.MaxTokenLength > 0 && builder.Len() > l.config.MaxTokenLength {
			break
		}
	}
	
	return builder.String()
}

// readTemplateString lee template strings (`string`)
func (l *Lexer) readTemplateString() string {
	builder := utils.NewStringLiteralBuilder('`')
	l.readChar() // salta la comilla inicial `
	
	for l.ch != '`' && l.ch != 0 {
		if l.ch == '\\' {
			l.readChar()
			builder.WriteEscape(l.ch)
		} else if l.ch == '$' && l.peekChar() == '{' {
			// Template literal interpolation ${...}
			builder.WriteByte(l.ch)
			l.readChar()
			builder.WriteByte(l.ch)
			l.readChar()
			
			// Leer hasta encontrar }
			braceCount := 1
			for braceCount > 0 && l.ch != 0 {
				if l.ch == '{' {
					braceCount++
				} else if l.ch == '}' {
					braceCount--
				}
				builder.WriteByte(l.ch)
				l.readChar()
			}
		} else {
			builder.WriteByte(l.ch)
			l.readChar()
		}
		
		if l.config.MaxTokenLength > 0 && builder.Len() > l.config.MaxTokenLength {
			break
		}
	}
	
	return builder.String()
}

// readLineComment lee un comentario de línea
func (l *Lexer) readLineComment() string {
	builder := utils.NewCommentBuilder("line")
	
	// Saltar //
	l.readChar()
	l.readChar()
	
	for l.ch != '\n' && l.ch != 0 {
		builder.WriteByte(l.ch)
		l.readChar()
	}
	
	return builder.String()
}

// readBlockComment lee un comentario de bloque
func (l *Lexer) readBlockComment() string {
	builder := utils.NewCommentBuilder("block")
	
	// Saltar /*
	l.readChar()
	l.readChar()
	
	for {
		if l.ch == '*' && l.peekChar() == '/' {
			l.readChar() // salta '*'
			l.readChar() // salta '/'
			break
		}
		if l.ch == 0 {
			break
		}
		builder.WriteByte(l.ch)
		l.readChar()
	}
	
	return builder.String()
}

// readRegexLiteral lee expresiones regulares /pattern/flags
func (l *Lexer) readRegexLiteral() string {
	builder := utils.NewStringLiteralBuilder('/')
	l.readChar() // salta la primera /
	
	for l.ch != '/' && l.ch != 0 && l.ch != '\n' {
		if l.ch == '\\' {
			builder.WriteByte(l.ch)
			l.readChar()
			if l.ch != 0 {
				builder.WriteByte(l.ch)
				l.readChar()
			}
		} else {
			builder.WriteByte(l.ch)
			l.readChar()
		}
	}
	
	// Leer flags (g, i, m, etc.)
	if l.ch == '/' {
		l.readChar()
		for utils.IsLetter(l.ch) {
			builder.WriteByte(l.ch)
			l.readChar()
		}
	}
	
	return builder.String()
}

// NextToken retorna el siguiente token optimizado
func (l *Lexer) NextToken() types.Token {
	var tok types.Token
	
	if l.config.SkipWhitespace {
		l.skipWhitespace()
	}
	
	// Manejar comentarios
	if l.ch == '/' && (l.peekChar() == '/' || l.peekChar() == '*') {
		if l.config.SkipComments {
			l.skipComments()
			if l.config.SkipWhitespace {
				l.skipWhitespace()
			}
		} else {
			// Retornar comentario como token
			pos := l.getCurrentPosition()
			var literal string
			var tokenType types.TokenType
			
			if l.peekChar() == '/' {
				literal = l.readLineComment()
				tokenType = types.COMMENT_LINE
			} else {
				literal = l.readBlockComment()
				tokenType = types.COMMENT_BLOCK
			}
			
			return types.Token{
				Type:     tokenType,
				Literal:  literal,
				Position: pos,
			}
		}
	}
	
	pos := l.getCurrentPosition()
	
	switch l.ch {
	case '=':
		if l.peekChar() == '=' {
			if l.peekCharN(2) == '=' {
				// ===
				l.readChar()
				l.readChar()
				tok = types.Token{Type: types.STRICT_EQ, Literal: "===", Position: pos}
			} else {
				// ==
				l.readChar()
				tok = types.Token{Type: types.EQ, Literal: "==", Position: pos}
			}
		} else if l.peekChar() == '>' {
			// =>
			l.readChar()
			tok = types.Token{Type: types.ARROW, Literal: "=>", Position: pos}
		} else {
			tok = l.newToken(types.ASSIGN, l.ch, pos)
		}
	case '+':
		if l.peekChar() == '+' {
			l.readChar()
			tok = types.Token{Type: types.INCREMENT, Literal: "++", Position: pos}
		} else if l.peekChar() == '=' {
			l.readChar()
			tok = types.Token{Type: types.PLUS_ASSIGN, Literal: "+=", Position: pos}
		} else {
			tok = l.newToken(types.PLUS, l.ch, pos)
		}
	case '-':
		if l.peekChar() == '-' {
			l.readChar()
			tok = types.Token{Type: types.DECREMENT, Literal: "--", Position: pos}
		} else if l.peekChar() == '=' {
			l.readChar()
			tok = types.Token{Type: types.MINUS_ASSIGN, Literal: "-=", Position: pos}
		} else {
			tok = l.newToken(types.MINUS, l.ch, pos)
		}
	case '*':
		if l.peekChar() == '*' {
			l.readChar()
			if l.peekChar() == '=' {
				l.readChar()
				tok = types.Token{Type: types.POWER_ASSIGN, Literal: "**=", Position: pos}
			} else {
				tok = types.Token{Type: types.POWER, Literal: "**", Position: pos}
			}
		} else if l.peekChar() == '=' {
			l.readChar()
			tok = types.Token{Type: types.MULTIPLY_ASSIGN, Literal: "*=", Position: pos}
		} else {
			tok = l.newToken(types.MULTIPLY, l.ch, pos)
		}
	case '/':
		if l.peekChar() == '=' {
			l.readChar()
			tok = types.Token{Type: types.DIVIDE_ASSIGN, Literal: "/=", Position: pos}
		} else {
			// Podría ser una expresión regular o división
			// Por simplicidad, tratamos como división
			tok = l.newToken(types.DIVIDE, l.ch, pos)
		}
	case '%':
		if l.peekChar() == '=' {
			l.readChar()
			tok = types.Token{Type: types.MODULO_ASSIGN, Literal: "%=", Position: pos}
		} else {
			tok = l.newToken(types.MODULO, l.ch, pos)
		}
	case '!':
		if l.peekChar() == '=' {
			if l.peekCharN(2) == '=' {
				// !==
				l.readChar()
				l.readChar()
				tok = types.Token{Type: types.STRICT_NOT_EQ, Literal: "!==", Position: pos}
			} else {
				// !=
				l.readChar()
				tok = types.Token{Type: types.NOT_EQ, Literal: "!=", Position: pos}
			}
		} else {
			tok = l.newToken(types.NOT, l.ch, pos)
		}
	case '<':
		if l.peekChar() == '=' {
			l.readChar()
			tok = types.Token{Type: types.LTE, Literal: "<=", Position: pos}
		} else if l.peekChar() == '<' {
			l.readChar()
			if l.peekChar() == '=' {
				l.readChar()
				tok = types.Token{Type: types.LEFT_SHIFT_ASSIGN, Literal: "<<=", Position: pos}
			} else {
				tok = types.Token{Type: types.LEFT_SHIFT, Literal: "<<", Position: pos}
			}
		} else {
			tok = l.newToken(types.LT, l.ch, pos)
		}
	case '>':
		if l.peekChar() == '=' {
			l.readChar()
			tok = types.Token{Type: types.GTE, Literal: ">=", Position: pos}
		} else if l.peekChar() == '>' {
			l.readChar()
			if l.peekChar() == '>' {
				l.readChar()
				if l.peekChar() == '=' {
					l.readChar()
					tok = types.Token{Type: types.UNSIGNED_RIGHT_SHIFT_ASSIGN, Literal: ">>>=", Position: pos}
				} else {
					tok = types.Token{Type: types.UNSIGNED_RIGHT_SHIFT, Literal: ">>>", Position: pos}
				}
			} else if l.peekChar() == '=' {
				l.readChar()
				tok = types.Token{Type: types.RIGHT_SHIFT_ASSIGN, Literal: ">>=", Position: pos}
			} else {
				tok = types.Token{Type: types.RIGHT_SHIFT, Literal: ">>", Position: pos}
			}
		} else {
			tok = l.newToken(types.GT, l.ch, pos)
		}
	case '&':
		if l.peekChar() == '&' {
			l.readChar()
			if l.peekChar() == '=' {
				l.readChar()
				tok = types.Token{Type: types.AND_ASSIGN, Literal: "&&=", Position: pos}
			} else {
				tok = types.Token{Type: types.AND, Literal: "&&", Position: pos}
			}
		} else if l.peekChar() == '=' {
			l.readChar()
			tok = types.Token{Type: types.BITWISE_AND_ASSIGN, Literal: "&=", Position: pos}
		} else {
			tok = types.Token{Type: types.BITWISE_AND, Literal: "&", Position: pos}
		}
	case '|':
		if l.peekChar() == '|' {
			l.readChar()
			if l.peekChar() == '=' {
				l.readChar()
				tok = types.Token{Type: types.OR_ASSIGN, Literal: "||=", Position: pos}
			} else {
				tok = types.Token{Type: types.OR, Literal: "||", Position: pos}
			}
		} else if l.peekChar() == '=' {
			l.readChar()
			tok = types.Token{Type: types.BITWISE_OR_ASSIGN, Literal: "|=", Position: pos}
		} else {
			tok = types.Token{Type: types.BITWISE_OR, Literal: "|", Position: pos}
		}
	case '^':
		if l.peekChar() == '=' {
			l.readChar()
			tok = types.Token{Type: types.BITWISE_XOR_ASSIGN, Literal: "^=", Position: pos}
		} else {
			tok = types.Token{Type: types.BITWISE_XOR, Literal: "^", Position: pos}
		}
	case '~':
		tok = types.Token{Type: types.BITWISE_NOT, Literal: "~", Position: pos}
	case '?':
		if l.peekChar() == '?' {
			l.readChar()
			if l.peekChar() == '=' {
				l.readChar()
				tok = types.Token{Type: types.NULLISH_ASSIGN, Literal: "??=", Position: pos}
			} else {
				tok = types.Token{Type: types.NULLISH_COALESCING, Literal: "??", Position: pos}
			}
		} else if l.peekChar() == '.' {
			l.readChar()
			tok = types.Token{Type: types.OPTIONAL_CHAINING, Literal: "?.", Position: pos}
		} else {
			tok = l.newToken(types.QUESTION, l.ch, pos)
		}
	case '.':
		if l.peekChar() == '.' && l.peekCharN(2) == '.' {
			l.readChar()
			l.readChar()
			tok = types.Token{Type: types.SPREAD, Literal: "...", Position: pos}
		} else if utils.IsDigit(l.peekChar()) {
			// Número que empieza con punto (.5)
			literal, tokenType := l.readNumber()
			tok = types.Token{Type: tokenType, Literal: "." + literal, Position: pos}
			l.tokenCount++
			return tok
		} else {
			tok = l.newToken(types.DOT, l.ch, pos)
		}
	case ',':
		tok = l.newToken(types.COMMA, l.ch, pos)
	case ';':
		tok = l.newToken(types.SEMICOLON, l.ch, pos)
	case ':':
		tok = l.newToken(types.COLON, l.ch, pos)
	case '(':
		tok = l.newToken(types.LPAREN, l.ch, pos)
	case ')':
		tok = l.newToken(types.RPAREN, l.ch, pos)
	case '{':
		tok = l.newToken(types.LBRACE, l.ch, pos)
	case '}':
		tok = l.newToken(types.RBRACE, l.ch, pos)
	case '[':
		tok = l.newToken(types.LBRACKET, l.ch, pos)
	case ']':
		tok = l.newToken(types.RBRACKET, l.ch, pos)
	case '"', '\'':
		tok.Type = types.STRING
		tok.Literal = l.readString()
		tok.Position = pos
	case '`':
		tok.Type = types.TEMPLATE_STRING
		tok.Literal = l.readTemplateString()
		tok.Position = pos
	case 0:
		tok.Literal = ""
		tok.Type = types.EOF
		tok.Position = pos
	default:
		if utils.IsValidIdentifierStart(l.ch) {
			tok.Literal = l.readIdentifier()
			tok.Type = types.LookupIdent(tok.Literal)
			tok.Position = pos
			l.tokenCount++
			return tok // return early porque readIdentifier() ya llamó readChar()
		} else if utils.IsDigit(l.ch) {
			var tokenType types.TokenType
			tok.Literal, tokenType = l.readNumber()
			tok.Type = tokenType
			tok.Position = pos
			l.tokenCount++
			return tok // return early porque readNumber() ya llamó readChar()
		} else {
			tok = l.newToken(types.ILLEGAL, l.ch, pos)
		}
	}
	
	l.readChar()
	l.tokenCount++
	return tok
}

// GetAllTokens retorna todos los tokens del input
func (l *Lexer) GetAllTokens() []types.Token {
	tokens := make([]types.Token, 0, 100) // pre-asigna capacidad estimada
	
	for {
		tok := l.NextToken()
		tokens = append(tokens, tok)
		if tok.Type == types.EOF {
			break
		}
	}
	
	return tokens
}

// GetPosition retorna la posición actual del lexer
func (l *Lexer) GetPosition() types.Position {
	return l.getCurrentPosition()
}

// Reset reinicia el lexer con nuevo input
func (l *Lexer) Reset(input string) {
	l.input = input
	l.position = 0
	l.readPosition = 0
	l.line = 1
	l.column = 0
	l.tokenCount = 0
	l.readChar()
}

// GetTokenCount retorna el número de tokens procesados
func (l *Lexer) GetTokenCount() int {
	return l.tokenCount
}

// GetConfig retorna la configuración actual
func (l *Lexer) GetConfig() LexerConfig {
	return l.config
}

// SetConfig actualiza la configuración
func (l *Lexer) SetConfig(config LexerConfig) {
	l.config = config
}

// newToken crea un nuevo token
func (l *Lexer) newToken(tokenType types.TokenType, ch byte, pos types.Position) types.Token {
	return types.Token{Type: tokenType, Literal: string(ch), Position: pos}
}

// HasMoreTokens verifica si hay más tokens por procesar
func (l *Lexer) HasMoreTokens() bool {
	return l.position < len(l.input)
}

// GetStats retorna estadísticas del lexer
func (l *Lexer) GetStats() LexerStats {
	return LexerStats{
		TokensProcessed: l.tokenCount,
		CurrentLine:     l.line,
		CurrentColumn:   l.column,
		InputLength:     len(l.input),
		Position:        l.position,
		Progress:        float64(l.position) / float64(len(l.input)) * 100,
	}
}

// LexerStats estadísticas del lexer
type LexerStats struct {
	TokensProcessed int     `json:"tokensProcessed"`
	CurrentLine     int     `json:"currentLine"`
	CurrentColumn   int     `json:"currentColumn"`
	InputLength     int     `json:"inputLength"`
	Position        int     `json:"position"`
	Progress        float64 `json:"progress"` // Porcentaje de progreso
}

// IsAtEnd verifica si el lexer está al final del input
func (l *Lexer) IsAtEnd() bool {
	return l.ch == 0 || l.position >= len(l.input)
}

// Peek retorna el carácter actual sin consumirlo
func (l *Lexer) Peek() byte {
	return l.ch
}

// GetCurrentLine retorna la línea actual completa
func (l *Lexer) GetCurrentLine() string {
	start := l.position
	for start > 0 && l.input[start-1] != '\n' {
		start--
	}
	
	end := l.position
	for end < len(l.input) && l.input[end] != '\n' {
		end++
	}
	
	if start < len(l.input) && end <= len(l.input) {
		return l.input[start:end]
	}
	return ""
}

// GetContext retorna el contexto alrededor de la posición actual
func (l *Lexer) GetContext(radius int) string {
	start := l.position - radius
	if start < 0 {
		start = 0
	}
	
	end := l.position + radius
	if end > len(l.input) {
		end = len(l.input)
	}
	
	return l.input[start:end]
}

// TokenizeAll conveniencia para obtener todos los tokens sin EOF
func (l *Lexer) TokenizeAll() []types.Token {
	tokens := make([]types.Token, 0, 100)
	
	for {
		tok := l.NextToken()
		if tok.Type == types.EOF {
			break
		}
		tokens = append(tokens, tok)
	}
	
	return tokens
}

// ValidateInput verifica que el input sea válido
func (l *Lexer) ValidateInput() []ValidationError {
	var errors []ValidationError
	oldPos := l.position
	oldReadPos := l.readPosition
	oldCh := l.ch
	oldLine := l.line
	oldColumn := l.column
	
	// Resetear para validar desde el inicio
	l.Reset(l.input)
	
	for !l.IsAtEnd() {
		tok := l.NextToken()
		if tok.Type == types.ILLEGAL {
			errors = append(errors, ValidationError{
				Position: tok.Position,
				Message:  "Carácter ilegal: " + tok.Literal,
				Type:     "ILLEGAL_CHARACTER",
			})
		}
	}
	
	// Restaurar posición original
	l.position = oldPos
	l.readPosition = oldReadPos
	l.ch = oldCh
	l.line = oldLine
	l.column = oldColumn
	
	return errors
}

// ValidationError representa un error de validación
type ValidationError struct {
	Position types.Position `json:"position"`
	Message  string         `json:"message"`
	Type     string         `json:"type"`
}

// Error implementa la interfaz error
func (ve ValidationError) Error() string {
	return ve.Message
}