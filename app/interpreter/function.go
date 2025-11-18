package interpreter

import (
	"fmt"
	"time"

	"github.com/codecrafters-io/interpreter-starter-go/app/parser"
)

// Lox function representation for interpreter.
type LoxFunction struct {
	Lexeme   string
	arity    int // number of arguments
	call     func(arguments []any) any
	toString func() string
}

// Define global / foreign / builtin functions for Lox
func defineGlobalFunctions() {
	globalEnvironment.bindings["clock"] = &LoxFunction{
		Lexeme: "clock",
		arity:  0,
		call: func(arguments []any) any {
			time_in_seconds := float64(time.Now().UnixMilli()) / 1000.0
			return time_in_seconds
		},
		toString: func() string {
			return "<fn clock>"
		},
	}
}

func constructLoxFunction(func_ast_node parser.FunctionAstNode, block *parser.AstNode) *LoxFunction {
	return &LoxFunction{
		Lexeme: func_ast_node.Name.Lexeme,
		arity:  len(func_ast_node.Parameters),
		call: func(arguments []any) any {
			func_environment := initializeEnvironment(globalEnvironment)
			for idx, param := range func_ast_node.Parameters {
				func_environment.bindings[param.Lexeme] = arguments[idx]
			}

			executeBlock(block, func_environment)

			return nil
		},
		toString: func() string {
			return fmt.Sprintf("<fn %s>", func_ast_node.Name.Lexeme)
		},
	}
}

func callLoxFunction(callee *LoxFunction, arguments []any) any {
	return callee.call(arguments)
}

// Check if the evaluated expression is a Lox callable
func isExpressionCallable(evaluated_expr any) (*LoxFunction, bool) {
	if evaluated_expr == nil {
		return nil, false
	}

	switch callee := evaluated_expr.(type) {
	case bool:
		return nil, false
	case float64:
		return nil, false
	case string:
		return nil, false
	case *LoxFunction:
		return callee, true
	default:
		return nil, false
	}
}
