package utils

import "unicode"

// Validadores de caracteres optimizados

// IsLetter verifica si el carácter es una letra válida para identificadores
func IsLetter(ch byte) bool {
	return ('a' <= ch && ch <= 'z') || 
		   ('A' <= ch && ch <= 'Z') || 
		   ch == '_' || 
		   ch == '$'
}

// IsDigit verifica si el carácter es un dígito
func IsDigit(ch byte) bool {
	return '0' <= ch && ch <= '9'
}

// IsAlphaNumeric verifica si el carácter es alfanumérico
func IsAlphaNumeric(ch byte) bool {
	return IsLetter(ch) || IsDigit(ch)
}

// IsWhitespace verifica si el carácter es un espacio en blanco
func IsWhitespace(ch byte) bool {
	return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r'
}

// IsNewline verifica si el carácter es un salto de línea
func IsNewline(ch byte) bool {
	return ch == '\n' || ch == '\r'
}

// IsQuote verifica si el carácter es una comilla
func IsQuote(ch byte) bool {
	return ch == '"' || ch == '\''
}

// IsValidIdentifierStart verifica si el carácter puede iniciar un identificador
func IsValidIdentifierStart(ch byte) bool {
	return IsLetter(ch)
}

// IsValidIdentifierPart verifica si el carácter puede ser parte de un identificador
func IsValidIdentifierPart(ch byte) bool {
	return IsLetter(ch) || IsDigit(ch)
}

// Funciones para caracteres Unicode (para futuras extensiones)

// IsUnicodeLetterOrDigit verifica si un rune es letra o dígito Unicode
func IsUnicodeLetterOrDigit(r rune) bool {
	return unicode.IsLetter(r) || unicode.IsDigit(r)
}

// IsUnicodeLetter verifica si un rune es una letra Unicode
func IsUnicodeLetter(r rune) bool {
	return unicode.IsLetter(r)
}

// IsUnicodeDigit verifica si un rune es un dígito Unicode
func IsUnicodeDigit(r rune) bool {
	return unicode.IsDigit(r)
}

// Utilidades para escape characters

// GetEscapeChar retorna el carácter escape correspondiente
func GetEscapeChar(ch byte) byte {
	switch ch {
	case 'n':
		return '\n'
	case 't':
		return '\t'
	case 'r':
		return '\r'
	case 'b':
		return '\b'
	case 'f':
		return '\f'
	case 'v':
		return '\v'
	case '\\':
		return '\\'
	case '"':
		return '"'
	case '\'':
		return '\''
	case '0':
		return '\x00'
	default:
		return ch // Si no es reconocido, retorna el mismo carácter
	}
}

// IsEscapeChar verifica si un carácter es un escape válido
func IsEscapeChar(ch byte) bool {
	switch ch {
	case 'n', 't', 'r', 'b', 'f', 'v', '\\', '"', '\'', '0':
		return true
	default:
		return false
	}
}

// Utilidades para números

// IsHexDigit verifica si el carácter es un dígito hexadecimal
func IsHexDigit(ch byte) bool {
	return IsDigit(ch) || 
		   ('a' <= ch && ch <= 'f') || 
		   ('A' <= ch && ch <= 'F')
}

// IsOctalDigit verifica si el carácter es un dígito octal
func IsOctalDigit(ch byte) bool {
	return '0' <= ch && ch <= '7'
}

// IsBinaryDigit verifica si el carácter es un dígito binario
func IsBinaryDigit(ch byte) bool {
	return ch == '0' || ch == '1'
}

// Utilidades para operadores

// IsOperatorChar verifica si el carácter puede ser parte de un operador
func IsOperatorChar(ch byte) bool {
	switch ch {
	case '+', '-', '*', '/', '%', '=', '!', '<', '>', '&', '|', '^', '~':
		return true
	default:
		return false
	}
}

// IsDelimiter verifica si el carácter es un delimitador
func IsDelimiter(ch byte) bool {
	switch ch {
	case ',', ';', ':', '.', '?':
		return true
	default:
		return false
	}
}

// IsBracket verifica si el carácter es un paréntesis o llave
func IsBracket(ch byte) bool {
	switch ch {
	case '(', ')', '[', ']', '{', '}':
		return true
	default:
		return false
	}
}

// Utilidades de conversión

// HexToDecimal convierte un carácter hexadecimal a su valor decimal
func HexToDecimal(ch byte) int {
	if IsDigit(ch) {
		return int(ch - '0')
	}
	if 'a' <= ch && ch <= 'f' {
		return int(ch - 'a' + 10)
	}
	if 'A' <= ch && ch <= 'F' {
		return int(ch - 'A' + 10)
	}
	return -1 // Valor inválido
}

// OctalToDecimal convierte un carácter octal a su valor decimal
func OctalToDecimal(ch byte) int {
	if IsOctalDigit(ch) {
		return int(ch - '0')
	}
	return -1 // Valor inválido
}