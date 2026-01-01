package resolver

import (
	"fmt"
	"os"

	"github.com/codecrafters-io/interpreter-starter-go/app/loxerrors"
	"github.com/codecrafters-io/interpreter-starter-go/app/parser"

	//lint:ignore ST1001 I dont care
	. "github.com/codecrafters-io/interpreter-starter-go/app/token"
)

// Stack representing scope. max limit set to 100 (arbitary)
var scope_stack []map[string]bool = make([]map[string]bool, 100)
var scope_top int = -1

func beginScope() {
	scope_top += 1
	scope_stack[scope_top] = make(map[string]bool)
}

func endScope() {
	scope_stack[scope_top] = nil
	scope_top -= 1
}

// Entry point of resolver takes a list of statements and does resolving and binding at compile time.
func Resolve(statements []*parser.AstNode) {
	for _, statement := range statements {
		resolveAst(statement)
	}
}

// Resolve individual nodes of AST
func resolveAst(node *parser.AstNode) error {
	var err error
	switch node.Type {
	case parser.BLOCK:
		err = resolveBlock(node)

	case parser.VARDECLR:
		err = resolveVarDecr(node)

	case parser.VARIABLE:
		err = resolveVariable(node)
	}

	// DELETE THIS
	fmt.Println(os.Args)
	loxerrors.GetErrorState()

	return err
}

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

func resolveLocal(node *parser.AstNode, name Token) {
	for i := len(scope_stack) - 1; i >= 0; i-- {
		if _, ok := scope_stack[i][name.Lexeme]; ok {
			resolve(node, len(scope_stack)-1-i)
		}
	}
}

func resolve(node *parser.AstNode, jumps int) {

}

// Resolve variable declaration
func resolveVarDecr(node *parser.AstNode) error {
	if len(node.Children) != 1 {
		return fmt.Errorf("resolver error: not exactly 1 child of node type var declaration.")
	}
	if var_name, ok := node.Representation.(string); ok {
		declare(var_name)
		// only declare and dont define for resolve to catch bugs such as 'var a = a;'
		err := resolveAst(node.Children[0])
		define(var_name)
		return err
	}
	return fmt.Errorf("resolver error: node representation for var declaration is not string.")
}

// Resolve Block of statement, which is under a new scope.
func resolveBlock(node *parser.AstNode) error {
	beginScope()
	for _, statement := range node.Children {
		err := resolveBlock(statement)
		if err != nil {
			return err
		}
	}
	endScope()
	return nil
}

func declare(identifier_name string) {
	if scope_top == -1 { // empty scope stack case
		return
	}

	scope := scope_stack[scope_top]
	scope[identifier_name] = false // put in scope but not yet processed/resolved
}

func define(identifier_name string) {
	if scope_top == -1 {
		return
	}

	scope := scope_stack[scope_top]
	scope[identifier_name] = true // marked as processed/resolved
}
