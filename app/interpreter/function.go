package interpreter

import "fmt"

type LoxFunction struct {
	Lexeme string
	call   func() any
}

func callLoxFunction(callee any, arguments []any) error {
	lox_func, ok := callee.(LoxFunction)
	if !ok {
		return fmt.Errorf("function is not of type LoxFunction")
	}

	lox_func.call()
	return nil
}

// check if the result is callable
func isExpressionCallable(evaluated_expr any) bool {
	if evaluated_expr == nil {
		return false
	}

	switch evaluated_expr.(type) {
	case bool:
		return false
	case float64:
		return false
	case string:
		return false
	case LoxFunction:
		return true
	default:
		return false
	}
}
