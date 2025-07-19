package parser

import (
	"fmt"
	"js-analyzer/internal/lexer"
	"js-analyzer/internal/lexer/types"
	"strings"
)

// Node representa un nodo en el AST
type Node interface {
	String() string
	Type() string
	GetPosition() types.Position
}

// Statement representa una declaración
type Statement interface {
	Node
	statementNode()
}

// Expression representa una expresión
type Expression interface {
	Node
	expressionNode()
}

// Program es el nodo raíz del AST
type Program struct {
	Statements []Statement
	Position   types.Position
}

func (p *Program) String() string {
	var out strings.Builder
	for _, s := range p.Statements {
		out.WriteString(s.String())
	}
	return out.String()
}

func (p *Program) Type() string {
	return "Program"
}

func (p *Program) GetPosition() types.Position {
	return p.Position
}

// VarStatement representa declaraciones var, let, const
type VarStatement struct {
	Token      lexer.Token // var, let, const
	Name       *Identifier
	Value      Expression
	Position   types.Position
	VarType    string // "var", "let", "const"
}

func (vs *VarStatement) statementNode() {}
func (vs *VarStatement) String() string {
	var out strings.Builder
	out.WriteString(vs.VarType)
	out.WriteString(" ")
	out.WriteString(vs.Name.String())
	if vs.Value != nil {
		out.WriteString(" = ")
		out.WriteString(vs.Value.String())
	}
	out.WriteString(";")
	return out.String()
}

func (vs *VarStatement) Type() string {
	return "VarStatement"
}

func (vs *VarStatement) GetPosition() types.Position {
	return vs.Position
}

// FunctionStatement representa declaraciones de función
type FunctionStatement struct {
	Token      lexer.Token
	Name       *Identifier
	Parameters []*Identifier
	Body       *BlockStatement
	Position   types.Position
	IsAsync    bool
	IsArrow    bool
}

func (fs *FunctionStatement) statementNode() {}
func (fs *FunctionStatement) String() string {
	var out strings.Builder
	
	if fs.IsAsync {
		out.WriteString("async ")
	}
	
	if fs.IsArrow {
		out.WriteString("(")
		params := make([]string, len(fs.Parameters))
		for i, p := range fs.Parameters {
			params[i] = p.String()
		}
		out.WriteString(strings.Join(params, ", "))
		out.WriteString(") => ")
		if fs.Body != nil {
			out.WriteString(fs.Body.String())
		}
	} else {
		out.WriteString("function ")
		if fs.Name != nil {
			out.WriteString(fs.Name.String())
		}
		out.WriteString("(")
		params := make([]string, len(fs.Parameters))
		for i, p := range fs.Parameters {
			params[i] = p.String()
		}
		out.WriteString(strings.Join(params, ", "))
		out.WriteString(") ")
		if fs.Body != nil {
			out.WriteString(fs.Body.String())
		}
	}
	
	return out.String()
}

func (fs *FunctionStatement) Type() string {
	if fs.IsArrow {
		return "ArrowFunction"
	}
	return "FunctionStatement"
}

func (fs *FunctionStatement) GetPosition() types.Position {
	return fs.Position
}

// ReturnStatement representa declaraciones return
type ReturnStatement struct {
	Token       lexer.Token
	ReturnValue Expression
	Position    types.Position
}

func (rs *ReturnStatement) statementNode() {}
func (rs *ReturnStatement) String() string {
	var out strings.Builder
	out.WriteString("return")
	if rs.ReturnValue != nil {
		out.WriteString(" ")
		out.WriteString(rs.ReturnValue.String())
	}
	out.WriteString(";")
	return out.String()
}

func (rs *ReturnStatement) Type() string {
	return "ReturnStatement"
}

func (rs *ReturnStatement) GetPosition() types.Position {
	return rs.Position
}

// ExpressionStatement representa declaraciones de expresión
type ExpressionStatement struct {
	Token      lexer.Token
	Expression Expression
	Position   types.Position
}

func (es *ExpressionStatement) statementNode() {}
func (es *ExpressionStatement) String() string {
	if es.Expression != nil {
		return es.Expression.String()
	}
	return ""
}

func (es *ExpressionStatement) Type() string {
	return "ExpressionStatement"
}

func (es *ExpressionStatement) GetPosition() types.Position {
	return es.Position
}

// BlockStatement representa un bloque de declaraciones
type BlockStatement struct {
	Token      lexer.Token
	Statements []Statement
	Position   types.Position
}

func (bs *BlockStatement) statementNode() {}
func (bs *BlockStatement) String() string {
	var out strings.Builder
	out.WriteString("{")
	for _, s := range bs.Statements {
		out.WriteString(s.String())
	}
	out.WriteString("}")
	return out.String()
}

func (bs *BlockStatement) Type() string {
	return "BlockStatement"
}

func (bs *BlockStatement) GetPosition() types.Position {
	return bs.Position
}

// IfStatement representa declaraciones if
type IfStatement struct {
	Token       lexer.Token
	Condition   Expression
	Consequence *BlockStatement
	Alternative Statement
	Position    types.Position
}

func (ifs *IfStatement) statementNode() {}
func (ifs *IfStatement) String() string {
	var out strings.Builder
	out.WriteString("if (")
	out.WriteString(ifs.Condition.String())
	out.WriteString(") ")
	out.WriteString(ifs.Consequence.String())
	if ifs.Alternative != nil {
		out.WriteString(" else ")
		out.WriteString(ifs.Alternative.String())
	}
	return out.String()
}

func (ifs *IfStatement) Type() string {
	return "IfStatement"
}

func (ifs *IfStatement) GetPosition() types.Position {
	return ifs.Position
}

// WhileStatement representa bucles while
type WhileStatement struct {
	Token     lexer.Token
	Condition Expression
	Body      *BlockStatement
	Position  types.Position
}

func (ws *WhileStatement) statementNode() {}
func (ws *WhileStatement) String() string {
	var out strings.Builder
	out.WriteString("while (")
	out.WriteString(ws.Condition.String())
	out.WriteString(") ")
	out.WriteString(ws.Body.String())
	return out.String()
}

func (ws *WhileStatement) Type() string {
	return "WhileStatement"
}

func (ws *WhileStatement) GetPosition() types.Position {
	return ws.Position
}

// ForStatement representa bucles for
type ForStatement struct {
	Token       lexer.Token
	Init        Statement   // inicialización
	Condition   Expression  // condición
	Update      Expression  // actualización
	Body        *BlockStatement
	Position    types.Position
}

func (fs *ForStatement) statementNode() {}
func (fs *ForStatement) String() string {
	var out strings.Builder
	out.WriteString("for (")
	if fs.Init != nil {
		out.WriteString(fs.Init.String())
	}
	out.WriteString(" ")
	if fs.Condition != nil {
		out.WriteString(fs.Condition.String())
	}
	out.WriteString("; ")
	if fs.Update != nil {
		out.WriteString(fs.Update.String())
	}
	out.WriteString(") ")
	out.WriteString(fs.Body.String())
	return out.String()
}

func (fs *ForStatement) Type() string {
	return "ForStatement"
}

func (fs *ForStatement) GetPosition() types.Position {
	return fs.Position
}

// Identifier representa identificadores
type Identifier struct {
	Token    lexer.Token
	Value    string
	Position types.Position
}

func (i *Identifier) expressionNode() {}
func (i *Identifier) String() string {
	return i.Value
}

func (i *Identifier) Type() string {
	return "Identifier"
}

func (i *Identifier) GetPosition() types.Position {
	return i.Position
}

// IntegerLiteral representa números enteros
type IntegerLiteral struct {
	Token    lexer.Token
	Value    int64
	Position types.Position
}

func (il *IntegerLiteral) expressionNode() {}
func (il *IntegerLiteral) String() string {
	return il.Token.Literal
}

func (il *IntegerLiteral) Type() string {
	return "IntegerLiteral"
}

func (il *IntegerLiteral) GetPosition() types.Position {
	return il.Position
}

// FloatLiteral representa números flotantes
type FloatLiteral struct {
	Token    lexer.Token
	Value    float64
	Position types.Position
}

func (fl *FloatLiteral) expressionNode() {}
func (fl *FloatLiteral) String() string {
	return fl.Token.Literal
}

func (fl *FloatLiteral) Type() string {
	return "FloatLiteral"
}

func (fl *FloatLiteral) GetPosition() types.Position {
	return fl.Position
}

// StringLiteral representa strings
type StringLiteral struct {
	Token    lexer.Token
	Value    string
	Position types.Position
}

func (sl *StringLiteral) expressionNode() {}
func (sl *StringLiteral) String() string {
	return fmt.Sprintf(`"%s"`, sl.Value)
}

func (sl *StringLiteral) Type() string {
	return "StringLiteral"
}

func (sl *StringLiteral) GetPosition() types.Position {
	return sl.Position
}

// TemplateLiteral representa template strings
type TemplateLiteral struct {
	Token    lexer.Token
	Value    string
	Position types.Position
}

func (tl *TemplateLiteral) expressionNode() {}
func (tl *TemplateLiteral) String() string {
	return fmt.Sprintf("`%s`", tl.Value)
}

func (tl *TemplateLiteral) Type() string {
	return "TemplateLiteral"
}

func (tl *TemplateLiteral) GetPosition() types.Position {
	return tl.Position
}

// BooleanLiteral representa valores booleanos
type BooleanLiteral struct {
	Token    lexer.Token
	Value    bool
	Position types.Position
}

func (bl *BooleanLiteral) expressionNode() {}
func (bl *BooleanLiteral) String() string {
	return bl.Token.Literal
}

func (bl *BooleanLiteral) Type() string {
	return "BooleanLiteral"
}

func (bl *BooleanLiteral) GetPosition() types.Position {
	return bl.Position
}

// NullLiteral representa null
type NullLiteral struct {
	Token    lexer.Token
	Position types.Position
}

func (nl *NullLiteral) expressionNode() {}
func (nl *NullLiteral) String() string {
	return "null"
}

func (nl *NullLiteral) Type() string {
	return "NullLiteral"
}

func (nl *NullLiteral) GetPosition() types.Position {
	return nl.Position
}

// InfixExpression representa expresiones binarias
type InfixExpression struct {
	Token    lexer.Token
	Left     Expression
	Operator string
	Right    Expression
	Position types.Position
}

func (ie *InfixExpression) expressionNode() {}
func (ie *InfixExpression) String() string {
	var out strings.Builder
	out.WriteString("(")
	out.WriteString(ie.Left.String())
	out.WriteString(" " + ie.Operator + " ")
	out.WriteString(ie.Right.String())
	out.WriteString(")")
	return out.String()
}

func (ie *InfixExpression) Type() string {
	return "InfixExpression"
}

func (ie *InfixExpression) GetPosition() types.Position {
	return ie.Position
}

// PrefixExpression representa expresiones unarias
type PrefixExpression struct {
	Token    lexer.Token
	Operator string
	Right    Expression
	Position types.Position
}

func (pe *PrefixExpression) expressionNode() {}
func (pe *PrefixExpression) String() string {
	var out strings.Builder
	out.WriteString("(")
	out.WriteString(pe.Operator)
	out.WriteString(pe.Right.String())
	out.WriteString(")")
	return out.String()
}

func (pe *PrefixExpression) Type() string {
	return "PrefixExpression"
}

func (pe *PrefixExpression) GetPosition() types.Position {
	return pe.Position
}

// PostfixExpression representa expresiones postfix (++ --)
type PostfixExpression struct {
	Token    lexer.Token
	Left     Expression
	Operator string
	Position types.Position
}

func (pe *PostfixExpression) expressionNode() {}
func (pe *PostfixExpression) String() string {
	var out strings.Builder
	out.WriteString("(")
	out.WriteString(pe.Left.String())
	out.WriteString(pe.Operator)
	out.WriteString(")")
	return out.String()
}

func (pe *PostfixExpression) Type() string {
	return "PostfixExpression"
}

func (pe *PostfixExpression) GetPosition() types.Position {
	return pe.Position
}

// CallExpression representa llamadas a función
type CallExpression struct {
	Token     lexer.Token
	Function  Expression
	Arguments []Expression
	Position  types.Position
}

func (ce *CallExpression) expressionNode() {}
func (ce *CallExpression) String() string {
	var out strings.Builder
	args := make([]string, len(ce.Arguments))
	for i, a := range ce.Arguments {
		args[i] = a.String()
	}
	out.WriteString(ce.Function.String())
	out.WriteString("(")
	out.WriteString(strings.Join(args, ", "))
	out.WriteString(")")
	return out.String()
}

func (ce *CallExpression) Type() string {
	return "CallExpression"
}

func (ce *CallExpression) GetPosition() types.Position {
	return ce.Position
}

// AssignmentExpression representa asignaciones
type AssignmentExpression struct {
	Token    lexer.Token
	Name     *Identifier
	Operator string
	Value    Expression
	Position types.Position
}

func (ae *AssignmentExpression) expressionNode() {}
func (ae *AssignmentExpression) String() string {
	var out strings.Builder
	out.WriteString(ae.Name.String())
	out.WriteString(" ")
	out.WriteString(ae.Operator)
	out.WriteString(" ")
	out.WriteString(ae.Value.String())
	return out.String()
}

func (ae *AssignmentExpression) Type() string {
	return "AssignmentExpression"
}

func (ae *AssignmentExpression) GetPosition() types.Position {
	return ae.Position
}

// MemberExpression representa acceso a propiedades (obj.prop, obj[prop])
type MemberExpression struct {
	Token    lexer.Token
	Object   Expression
	Property Expression
	Computed bool // true para obj[prop], false para obj.prop
	Position types.Position
}

func (me *MemberExpression) expressionNode() {}
func (me *MemberExpression) String() string {
	var out strings.Builder
	out.WriteString(me.Object.String())
	if me.Computed {
		out.WriteString("[")
		out.WriteString(me.Property.String())
		out.WriteString("]")
	} else {
		out.WriteString(".")
		out.WriteString(me.Property.String())
	}
	return out.String()
}

func (me *MemberExpression) Type() string {
	return "MemberExpression"
}

func (me *MemberExpression) GetPosition() types.Position {
	return me.Position
}

// ArrayLiteral representa arrays [1, 2, 3]
type ArrayLiteral struct {
	Token    lexer.Token
	Elements []Expression
	Position types.Position
}

func (al *ArrayLiteral) expressionNode() {}
func (al *ArrayLiteral) String() string {
	var out strings.Builder
	elements := make([]string, len(al.Elements))
	for i, e := range al.Elements {
		elements[i] = e.String()
	}
	out.WriteString("[")
	out.WriteString(strings.Join(elements, ", "))
	out.WriteString("]")
	return out.String()
}

func (al *ArrayLiteral) Type() string {
	return "ArrayLiteral"
}

func (al *ArrayLiteral) GetPosition() types.Position {
	return al.Position
}

// ObjectLiteral representa objetos {key: value}
type ObjectLiteral struct {
	Token    lexer.Token
	Pairs    map[Expression]Expression
	Position types.Position
}

func (ol *ObjectLiteral) expressionNode() {}
func (ol *ObjectLiteral) String() string {
	var out strings.Builder
	pairs := []string{}
	for key, value := range ol.Pairs {
		pairs = append(pairs, key.String()+": "+value.String())
	}
	out.WriteString("{")
	out.WriteString(strings.Join(pairs, ", "))
	out.WriteString("}")
	return out.String()
}

func (ol *ObjectLiteral) Type() string {
	return "ObjectLiteral"
}

func (ol *ObjectLiteral) GetPosition() types.Position {
	return ol.Position
}