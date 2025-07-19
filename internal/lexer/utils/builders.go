package utils

import (
	"strconv"
	"strings"
)

// StringBuilder pool para reutilización optimizada
var stringBuilderPool = make(chan *strings.Builder, 10)

// GetStringBuilder obtiene un builder del pool o crea uno nuevo
func GetStringBuilder() *strings.Builder {
	select {
	case builder := <-stringBuilderPool:
		builder.Reset()
		return builder
	default:
		builder := &strings.Builder{}
		builder.Grow(32) // Capacidad inicial optimizada
		return builder
	}
}

// PutStringBuilder devuelve un builder al pool
func PutStringBuilder(builder *strings.Builder) {
	if builder.Cap() < 1024 { // Evitar builders muy grandes en el pool
		select {
		case stringBuilderPool <- builder:
		default:
			// Pool lleno, descartar
		}
	}
}

// IdentifierBuilder constructor optimizado para identificadores
type IdentifierBuilder struct {
	builder *strings.Builder
}

// NewIdentifierBuilder crea un nuevo constructor de identificadores
func NewIdentifierBuilder() *IdentifierBuilder {
	return &IdentifierBuilder{
		builder: GetStringBuilder(),
	}
}

// WriteByte agrega un byte al identificador
func (ib *IdentifierBuilder) WriteByte(b byte) {
	ib.builder.WriteByte(b)
}

// String retorna el identificador construido
func (ib *IdentifierBuilder) String() string {
	result := ib.builder.String()
	PutStringBuilder(ib.builder)
	return result
}

// Len retorna la longitud actual del identificador
func (ib *IdentifierBuilder) Len() int {
	return ib.builder.Len()
}

// NumberBuilder constructor optimizado para números
type NumberBuilder struct {
	builder   *strings.Builder
	hasDecimal bool
	isFloat    bool
}

// NewNumberBuilder crea un nuevo constructor de números
func NewNumberBuilder() *NumberBuilder {
	return &NumberBuilder{
		builder:    GetStringBuilder(),
		hasDecimal: false,
		isFloat:    false,
	}
}

// WriteByte agrega un byte al número
func (nb *NumberBuilder) WriteByte(b byte) {
	nb.builder.WriteByte(b)
}

// WriteDecimalPoint agrega el punto decimal
func (nb *NumberBuilder) WriteDecimalPoint() {
	if !nb.hasDecimal {
		nb.builder.WriteByte('.')
		nb.hasDecimal = true
		nb.isFloat = true
	}
}

// IsFloat retorna si el número es flotante
func (nb *NumberBuilder) IsFloat() bool {
	return nb.isFloat
}

// String retorna el número construido
func (nb *NumberBuilder) String() string {
	result := nb.builder.String()
	PutStringBuilder(nb.builder)
	return result
}

// Len retorna la longitud actual del número
func (nb *NumberBuilder) Len() int {
	return nb.builder.Len()
}

// ToInt64 convierte el número a int64
func (nb *NumberBuilder) ToInt64() (int64, error) {
	if nb.isFloat {
		return 0, strconv.ErrSyntax
	}
	return strconv.ParseInt(nb.String(), 10, 64)
}

// ToFloat64 convierte el número a float64
func (nb *NumberBuilder) ToFloat64() (float64, error) {
	return strconv.ParseFloat(nb.String(), 64)
}

// StringLiteralBuilder constructor optimizado para strings literales
type StringLiteralBuilder struct {
	builder *strings.Builder
	quote   byte
}

// NewStringLiteralBuilder crea un nuevo constructor de strings
func NewStringLiteralBuilder(quote byte) *StringLiteralBuilder {
	builder := GetStringBuilder()
	builder.Grow(64) // Strings suelen ser más largos
	return &StringLiteralBuilder{
		builder: builder,
		quote:   quote,
	}
}

// WriteByte agrega un byte al string
func (sb *StringLiteralBuilder) WriteByte(b byte) {
	sb.builder.WriteByte(b)
}

// WriteEscape agrega un carácter escape
func (sb *StringLiteralBuilder) WriteEscape(escaped byte) {
	sb.builder.WriteByte(GetEscapeChar(escaped))
}

// WriteRaw agrega un byte sin procesamiento
func (sb *StringLiteralBuilder) WriteRaw(b byte) {
	sb.builder.WriteByte(b)
}

// String retorna el string construido
func (sb *StringLiteralBuilder) String() string {
	result := sb.builder.String()
	PutStringBuilder(sb.builder)
	return result
}

// Len retorna la longitud actual del string
func (sb *StringLiteralBuilder) Len() int {
	return sb.builder.Len()
}

// Quote retorna el tipo de comilla usado
func (sb *StringLiteralBuilder) Quote() byte {
	return sb.quote
}

// CommentBuilder constructor para comentarios
type CommentBuilder struct {
	builder     *strings.Builder
	commentType string // "line" o "block"
}

// NewCommentBuilder crea un nuevo constructor de comentarios
func NewCommentBuilder(commentType string) *CommentBuilder {
	return &CommentBuilder{
		builder:     GetStringBuilder(),
		commentType: commentType,
	}
}

// WriteByte agrega un byte al comentario
func (cb *CommentBuilder) WriteByte(b byte) {
	cb.builder.WriteByte(b)
}

// WriteString agrega un string al comentario
func (cb *CommentBuilder) WriteString(s string) {
	cb.builder.WriteString(s)
}

// String retorna el comentario construido
func (cb *CommentBuilder) String() string {
	result := cb.builder.String()
	PutStringBuilder(cb.builder)
	return result
}

// Type retorna el tipo de comentario
func (cb *CommentBuilder) Type() string {
	return cb.commentType
}

// Len retorna la longitud actual del comentario
func (cb *CommentBuilder) Len() int {
	return cb.builder.Len()
}

// BuilderStats estadísticas de uso de builders
type BuilderStats struct {
	IdentifiersBuilt int
	NumbersBuilt     int
	StringsBuilt     int
	CommentsBuilt    int
}

var stats BuilderStats

// GetBuilderStats retorna las estadísticas de uso
func GetBuilderStats() BuilderStats {
	return stats
}

// ResetBuilderStats reinicia las estadísticas
func ResetBuilderStats() {
	stats = BuilderStats{}
}