package interpreter

import "time"

type LoxFunction struct {
	Lexeme string
	arity  int // number of arguments
	call   func(arguments []any) any
}

func defineGlobalFunctions(global *EnvironmentNode) {
	global.environment["clock"] = LoxFunction{
		Lexeme: "clock",
		arity:  0,
		call: func(arguments []any) any {
			time_in_seconds := float64(time.Now().UnixMilli()) / 1000.0
			return time_in_seconds
		},
	}
}

func callLoxFunction(callee *LoxFunction, arguments []any) any {
	return callee.call(arguments)
}

// check if the result is callable
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
	case LoxFunction:
		return &callee, true
	default:
		return nil, false
	}
}
