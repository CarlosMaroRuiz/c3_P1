package types

import "fmt"

// Position representa la posición de un token en el código fuente
type Position struct {
	Line   int `json:"line"`   // línea (comenzando en 1)
	Column int `json:"column"` // columna (comenzando en 1)
	Offset int `json:"offset"` // offset absoluto en el archivo (comenzando en 0)
}

// NewPosition crea una nueva posición
func NewPosition(line, column, offset int) Position {
	return Position{
		Line:   line,
		Column: column,
		Offset: offset,
	}
}

// String retorna la representación en string de la posición
func (p Position) String() string {
	return fmt.Sprintf("Line: %d, Column: %d", p.Line, p.Column)
}

// IsValid verifica si la posición es válida
func (p Position) IsValid() bool {
	return p.Line > 0 && p.Column > 0 && p.Offset >= 0
}

// Before verifica si esta posición está antes que otra
func (p Position) Before(other Position) bool {
	if p.Line != other.Line {
		return p.Line < other.Line
	}
	return p.Column < other.Column
}

// After verifica si esta posición está después que otra
func (p Position) After(other Position) bool {
	if p.Line != other.Line {
		return p.Line > other.Line
	}
	return p.Column > other.Column
}

// Equal verifica si dos posiciones son iguales
func (p Position) Equal(other Position) bool {
	return p.Line == other.Line && p.Column == other.Column
}

// Range representa un rango en el código fuente
type Range struct {
	Start Position `json:"start"`
	End   Position `json:"end"`
}

// NewRange crea un nuevo rango
func NewRange(start, end Position) Range {
	return Range{Start: start, End: end}
}

// String retorna la representación en string del rango
func (r Range) String() string {
	return fmt.Sprintf("[%s - %s]", r.Start.String(), r.End.String())
}

// IsValid verifica si el rango es válido
func (r Range) IsValid() bool {
	return r.Start.IsValid() && r.End.IsValid() && !r.Start.After(r.End)
}

// Contains verifica si una posición está dentro del rango
func (r Range) Contains(pos Position) bool {
	return !pos.Before(r.Start) && !pos.After(r.End)
}

// Overlaps verifica si dos rangos se superponen
func (r Range) Overlaps(other Range) bool {
	return r.Contains(other.Start) || r.Contains(other.End) ||
		   other.Contains(r.Start) || other.Contains(r.End)
}

// Length retorna la longitud del rango en caracteres
func (r Range) Length() int {
	if !r.IsValid() {
		return 0
	}
	return r.End.Offset - r.Start.Offset
}