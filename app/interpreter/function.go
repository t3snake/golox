package interpreter

import (
	"fmt"
	"time"

	"github.com/codecrafters-io/interpreter-starter-go/app/parser"
	. "github.com/codecrafters-io/interpreter-starter-go/app/token"
)

// Use special error type to propogate error till the function call
type ErrReturnSignal struct {
	message string // will be return
	value   any    // will be the expression evaluated
}

// Satisfies error interface for ErrReturnSignal
func (e *ErrReturnSignal) Error() string {
	return e.message
}

// Lox function representation for interpreter.
type LoxFunction struct {
	Lexeme        string
	Parameters    []Token
	Block         *parser.AstNode
	IsInitializer bool
	arity         int // number of arguments
	call          func(arguments []any) any
	toString      func() string
}

// Define global / foreign / builtin functions for Lox
func defineGlobalFunctions() {
	global_environment.bindings["clock"] = &LoxFunction{
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

func constructLoxFunction(
	name string,
	parameters []Token,
	block *parser.AstNode,
	environment *EnvironmentNode,
	isInitializer bool,
) *LoxFunction {
	return &LoxFunction{
		Lexeme:        name,
		Parameters:    parameters, // For using an existing LoxFunction to create a modified LoxFunction
		Block:         block,      // to create a modified version of existing lox instance
		IsInitializer: isInitializer,
		arity:         len(parameters),
		call: func(arguments []any) any {
			func_environment := initializeEnvironment(environment)
			for idx, param := range parameters {
				func_environment.bindings[param.Lexeme] = arguments[idx]
			}

			_, err := executeBlock(block, func_environment)
			if err != nil {
				returnVal, ok := err.(*ErrReturnSignal)
				if !ok {
					return err
				}

				if isInitializer {
					// in case return; found in initializer, hijack and return instance instead of nil
					val, _ := environment.bindings["this"]
					return val
				}
				return returnVal.value
			}

			if isInitializer {
				// returns instance (previously binded this) always if initializer/constructor
				val, _ := environment.bindings["this"]
				return val
			}

			return nil
		},
		toString: func() string {
			return fmt.Sprintf("<fn %s>", name)
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
	case *LoxClass:
		// class constructor / instance call
		return &callee.LoxFunction, true
	default:
		return nil, false
	}
}
