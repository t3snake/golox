package interpreter

import (
	"fmt"
	//lint:ignore ST1001 I dont care
	"github.com/codecrafters-io/interpreter-starter-go/app/loxerrors"
	. "github.com/codecrafters-io/interpreter-starter-go/app/token"
)

type LoxClass struct {
	name        string
	methods     map[string]LoxFunction
	LoxFunction // embed LoxFunction struct since instance is callable Class()
}

type LoxInstance struct {
	class    *LoxClass
	fields   map[string]any
	toString func() string
}

func (inst *LoxInstance) get(property Token) (any, error) {
	// search properties or fields of class
	field_val, ok := inst.fields[property.Lexeme]
	if ok {
		return field_val, nil
	}

	// search method of the class
	method, ok := inst.class.methods[property.Lexeme]
	if ok {
		return &method, nil
	}
	return nil, loxerrors.RuntimeError(property,
		fmt.Sprintf("Undefined property %s.", property.Lexeme))
}

func (inst *LoxInstance) set(property Token, value any) {
	inst.fields[property.Lexeme] = value
}

func constructLoxClass(class_name string, methods map[string]LoxFunction) *LoxClass {
	class := &LoxClass{
		name:    class_name,
		methods: methods,
	}

	class.LoxFunction = LoxFunction{
		Lexeme: class_name,
		arity:  0,
		call: func(arguments []any) any {
			return LoxInstance{
				class:  class,
				fields: make(map[string]any),
				toString: func() string {
					return fmt.Sprintf("%s instance", class_name)
				},
			}
		},
		toString: func() string {
			return class_name
		},
	}

	return class
}
