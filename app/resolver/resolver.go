package resolver

import (
	"fmt"

	"github.com/codecrafters-io/interpreter-starter-go/app/interpreter"
	"github.com/codecrafters-io/interpreter-starter-go/app/loxerrors"
	"github.com/codecrafters-io/interpreter-starter-go/app/parser"

	//lint:ignore ST1001 I dont care
	. "github.com/codecrafters-io/interpreter-starter-go/app/token"
)

// Stack representing scope. max limit set to 100 (arbitary)
var scope_stack []map[string]bool = make([]map[string]bool, 100)

// Points to top of stack, -1 signifies empty
var scope_top int = -1

type FunctionType int

const (
	NONE_FN     FunctionType = 0
	FUNCTION    FunctionType = 1
	METHOD      FunctionType = 2
	INITIALIZER FunctionType = 3
)

// Current function type
var current_func_type FunctionType = NONE_FN

type ClassType int

const (
	NONE_CL     ClassType = 0
	CLASS_DECLR ClassType = 1
)

var current_class_type ClassType = NONE_CL

// Pushes a new scope into scope stack.
func beginScope() {
	scope_top += 1
	scope_stack[scope_top] = make(map[string]bool)
}

// Pops scope from scope stack.
func endScope() {
	if scope_top == -1 {
		return
	}
	scope_stack[scope_top] = nil
	scope_top -= 1
}

// Entry point of resolver package. Takes a list of statements and does resolving and binding at compile time.
func Resolve(statements []*parser.AstNode) error {
	for _, statement := range statements {
		err := resolveAst(statement)
		if err != nil {
			// expected errors are logged in loxerrors and does not immediately end execution,
			// thus can have multiple errors in different lines.
			return err
		}
	}

	return nil
}

// Resolve individual nodes of AST, can be used recursively on AST.
func resolveAst(node *parser.AstNode) error {
	var err error
	switch node.Type {
	// Statement cases

	case parser.BLOCK:
		err = resolveBlock(node)

	case parser.VARDECLR:
		err = resolveVarDecr(node)

	case parser.ASSIGNMENT:
		err = resolveAssignment(node)

	case parser.FNDECL:
		err = resolveFuncDeclr(node, FUNCTION)

	case parser.EXPRSTM, parser.PRINTSTM: // same struct as expression stmt
		err = resolveExprStmt(node)

	case parser.IFSTMT:
		err = resolveIfStmt(node)

	case parser.RETURNSTM:
		err = resolveReturnStmt(node)

	case parser.WHILESTM:
		err = resolveWhileStmt(node)

	case parser.FNCALL:
		err = resolveFuncCall(node)

	case parser.CLASSDECL:
		err = resolveClassDecl(node)

	// Expression cases

	case parser.THISNODE:
		err = resolveThisNode(node)

	case parser.SETTER:
		err = resolveSetter(node)

	case parser.GETTER:
		err = resolveGetter(node)

	case parser.VARIABLE:
		err = resolveVariable(node)

	case parser.BINARY:
		err = resolveBinaryExpr(node)

	case parser.LOGICALOP:
		err = resolveLogicalOpExpr(node)

	case parser.GROUP, parser.UNARY:
		err = resolveExprStmt(node) // same struct as expression stmt

	case parser.NUMBERNODE, parser.STRINGNODE, parser.TERMINAL:
		return nil

	default:
		return fmt.Errorf("resolver error: unexpected AST node %s", node.Type)
	}

	return err
}

// Resolve Block of statement. This is done under a newly pushed scope and pops scope after block is resolved.
func resolveBlock(node *parser.AstNode) error {
	beginScope()
	for _, statement := range node.Children {
		err := resolveAst(statement)
		if err != nil {
			return err
		}
	}
	endScope()
	return nil
}

// Resolve variable declaration. Declares to avoid using outer variable and defines to avoid using variable in initializer itself.
func resolveVarDecr(node *parser.AstNode) error {
	if len(node.Children) != 1 {
		return fmt.Errorf("resolver error: not exactly 1 child of node type var declaration.")
	}

	if var_name, ok := node.Representation.(Token); ok {
		declare(var_name)
		// only declare and dont define for resolve to catch bugs such as 'var a = a;'
		err := resolveAst(node.Children[0])
		define(var_name)
		return err
	}

	return fmt.Errorf("resolver error: node representation for var declaration is not Token.")
}

// Resolve a variable expression, ie. access of variable. Returns error if access is done in its own initialization.
func resolveVariable(node *parser.AstNode) error {
	var_token, ok := node.Representation.(Token)
	if !ok {
		return fmt.Errorf("resolver error: variable node representation not of type Token.")
	}
	if scope_top != -1 {
		scope := scope_stack[scope_top]
		var_status, ok := scope[var_token.Lexeme]
		if ok && !var_status {
			return loxerrors.RuntimeError(var_token, "Can't read local variable in its own initializer")
		}
	}

	resolveLocal(node, var_token)
	return nil
}

// Calculate the depth at which the expression node resolves and store it for use in interpreter.
func resolveLocal(expr_node *parser.AstNode, name Token) {
	for i := scope_top; i >= 0; i-- {
		if _, ok := scope_stack[i][name.Lexeme]; ok {
			interpreter.Resolve(expr_node, scope_top-i)
			return
		}
	}
}

// Resolve assignment statement, resolves initializer and then stores for use in interpreter.
func resolveAssignment(node *parser.AstNode) error {
	if len(node.Children) != 1 {
		return fmt.Errorf("resolver error: not exactly 1 child for assignment node.")
	}
	err := resolveAst(node.Children[0])
	if err != nil {
		return err
	}

	l_token, ok := node.Representation.(Token)
	if !ok {
		return fmt.Errorf("resolver error: representation for assignment node is not a token.")
	}
	resolveLocal(node, l_token)
	return nil
}

// Resolve function declaration statement.
func resolveFuncDeclr(node *parser.AstNode, func_type FunctionType) error {
	func_node, ok := node.Representation.(parser.FunctionAstNode)
	if !ok {
		return fmt.Errorf("resolver error: representation of func declaration statement is not of type FunctionAstNode.")
	}

	if func_type == METHOD && func_node.Name.Lexeme == "init" {
		func_type = INITIALIZER
	}

	enclosing_func_type := current_func_type
	current_func_type = func_type

	if len(node.Children) != 1 {
		return fmt.Errorf("resolver error: not exactly 1 child for function declaration.")
	}

	// can immediately define, since a function declaration can legally refer to itself before resolution
	// ie. recursion
	declare(func_node.Name)
	define(func_node.Name)

	beginScope()
	for _, param := range func_node.Parameters {
		declare(param)
		define(param) // can immediately define since initialize bug would not happen with func params
	}

	// This is different from interpreter where we only interpret body in a call
	// in case of resolver ie. static analysis, we immediately go through the body once
	if node.Children[0].Type == parser.BLOCK {
		// if block resolve as list of statement and not block since block creates a new scope
		// This is an implementation difference with the book Java implementation
		err := Resolve(node.Children[0].Children)
		if err != nil {
			return err
		}
	} else {
		err := resolveAst(node.Children[0]) // func body
		if err != nil {
			return err
		}
	}

	endScope()

	current_func_type = enclosing_func_type
	return nil
}

// Resolve expression statement
func resolveExprStmt(node *parser.AstNode) error {
	if len(node.Children) != 1 {
		return fmt.Errorf("resolver error: not exactly 1 child of expression statement.")
	}

	err := resolveAst(node.Children[0])
	return err
}

// Resolves if statement. Unlike interpreter, we resolve both branches if else branch existes.
func resolveIfStmt(node *parser.AstNode) error {
	if len(node.Children) != 3 {
		return fmt.Errorf("resolver error: if statement node does not have exactly 3 children.")
	}

	err := resolveAst(node.Children[0]) // condition
	if err != nil {
		return err
	}

	err = resolveAst(node.Children[1]) // then statement
	if err != nil {
		return err
	}

	if node.Children[2] != nil {
		err = resolveAst(node.Children[2]) // else statement if it exists.
		if err != nil {
			return err
		}
	}
	return nil
}

// Resolves return statement.
func resolveReturnStmt(node *parser.AstNode) error {
	if len(node.Children) != 1 {
		return fmt.Errorf("resolver error: not exactly 1 child of return statement.")
	}

	tok, ok := node.Representation.(Token)
	if !ok {
		return fmt.Errorf("resolver error: representation of return statement was not Token.")
	}

	if current_func_type == NONE_FN {
		loxerrors.RuntimeError(tok, "Can't return from top level code.")
	}

	if node.Children[0] != nil {
		if current_func_type == INITIALIZER {
			// cant have non nil/empty return for initializer/constructor
			loxerrors.RuntimeError(tok, "Can't return a value from initializer.")
			return nil
		}
		err := resolveAst(node.Children[0])
		return err
	}

	return nil
}

// Resolves while statement.
func resolveWhileStmt(node *parser.AstNode) error {
	if len(node.Children) != 2 {
		return fmt.Errorf("resolver error: not exactly 2 children for while statement node.")
	}

	err := resolveAst(node.Children[0]) // condition
	if err != nil {
		return err
	}

	err = resolveAst(node.Children[1]) // body or statement
	return err
}

// Resolves function call.
func resolveFuncCall(node *parser.AstNode) error {
	if len(node.Children) < 1 {
		return fmt.Errorf("resolver error: function call doesnt have atleast 1 children.")
	}

	err := resolveAst(node.Children[0]) // callee
	if err != nil {
		return err
	}

	for _, param := range node.Children[1:] {
		err = resolveAst(param)
		if err != nil {
			return err
		}
	}

	return nil
}

// Resolves class declaration
func resolveClassDecl(node *parser.AstNode) error {
	class_name, ok := node.Representation.(Token)
	if !ok {
		return fmt.Errorf("resolver error: representation for class node is not a token.")
	}

	if len(node.Children) < 1 {
		return fmt.Errorf("resolver error: not >= 1 children in class declaration node.")
	}

	declare(class_name)
	define(class_name)

	enclosing_class_type := current_class_type
	current_class_type = CLASS_DECLR

	superclass_node := node.Children[0]
	if superclass_node != nil {
		superclass, ok := superclass_node.Representation.(Token)
		if !ok {
			return fmt.Errorf("resolver error: superclass node representation is not a Token.")
		}

		if class_name.Lexeme == superclass.Lexeme {
			loxerrors.RuntimeError(superclass, "A class can't inherit from itself.")
		}

		err := resolveAst(superclass_node)
		if err != nil {
			return err
		}
	}

	beginScope()
	scope := scope_stack[scope_top]
	scope["this"] = true // add this in scope for further use as a normal variable

	for _, child := range node.Children[1:] {
		err := resolveFuncDeclr(child, METHOD)
		if err != nil {
			return err
		}
	}

	endScope()

	current_class_type = enclosing_class_type

	return nil
}

// Resolves this expressions.
func resolveThisNode(node *parser.AstNode) error {
	token, ok := node.Representation.(Token)
	if !ok {
		return fmt.Errorf("resolver error: representation of this node is not of type Token.")
	}

	if current_class_type != CLASS_DECLR {
		loxerrors.RuntimeError(token, "Can't use 'this' outside class.")
	}

	resolveLocal(node, token)
	return nil
}

// Resolves setter expressions.
func resolveSetter(node *parser.AstNode) error {
	if len(node.Children) != 2 {
		return fmt.Errorf("resolver error: not exactly 2 children of setter node")
	}

	err := resolveAst(node.Children[1]) // r value
	if err != nil {
		return err
	}

	return resolveAst(node.Children[0]) // l value
}

// Resolves getter expressions.
func resolveGetter(node *parser.AstNode) error {
	if len(node.Children) != 1 {
		return fmt.Errorf("resolver error: getter node does not have exactly 1 child.")
	}
	return resolveAst(node.Children[0])
}

// Resolves binary expressions.
func resolveBinaryExpr(node *parser.AstNode) error {
	if len(node.Children) != 2 {
		return fmt.Errorf("resolver error: not exactly 2 children for binary expression node.")
	}

	err := resolveAst(node.Children[0]) // left expr
	if err != nil {
		return err
	}

	err = resolveAst(node.Children[1]) // right expr
	return err
}

// Resolves logical operator expressions. Unlike interpreter, does not short circuit
// and resolves both left and right expressions
func resolveLogicalOpExpr(node *parser.AstNode) error {
	if len(node.Children) != 2 {
		return fmt.Errorf("resolver error: not exactly 2 children for logical operator expr node.")
	}

	err := resolveAst(node.Children[0]) // left expr
	if err != nil {
		return err
	}

	err = resolveAst(node.Children[1]) // right expr
	return err
}

// Puts a variable in scope and sets it as not processed. This is done to prevent outer variables to be referred during initialization.
func declare(identifier Token) {
	if scope_top == -1 { // empty scope stack case
		return
	}

	scope := scope_stack[scope_top]
	_, ok := scope[identifier.Lexeme]
	if ok {
		loxerrors.RuntimeError(identifier, "Already a variable with this name in scope.")
	}
	scope[identifier.Lexeme] = false // put in scope but not yet processed/resolved
}

// Puts a variable in scope and sets it as processed. This is done to prevent self variable usage during its initialization.
// Once it is defined, it can be safely accessed.
func define(identifier Token) {
	if scope_top == -1 {
		return
	}

	scope := scope_stack[scope_top]
	scope[identifier.Lexeme] = true // marked as processed/resolved
}
