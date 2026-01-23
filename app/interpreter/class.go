package interpreter

import (
	"fmt"
	//lint:ignore ST1001 I dont care
	"github.com/codecrafters-io/interpreter-starter-go/app/loxerrors"
	. "github.com/codecrafters-io/interpreter-starter-go/app/token"
)

type LoxClass struct {
	name        string
	LoxFunction // embed LoxFunction struct
}

type LoxInstance struct {
	fields   map[string]any
	toString func() string
}

func (inst *LoxInstance) get(property Token) (any, error) {
	field_val, ok := inst.fields[property.Lexeme]
	var err error = nil
	if !ok {
		err = loxerrors.RuntimeError(property,
			fmt.Sprintf("Undefined property %s.", property.Lexeme))
	}
	return field_val, err
}

func (inst *LoxInstance) set(property Token, value any) {
	inst.fields[property.Lexeme] = value
}

func constructLoxClass(class_name string) *LoxClass {
	return &LoxClass{
		name: class_name,
		LoxFunction: LoxFunction{
			Lexeme: class_name,
			arity:  0,
			call: func(arguments []any) any {
				return LoxInstance{
					fields: make(map[string]any),
					toString: func() string {
						return fmt.Sprintf("%s instance", class_name)
					},
				}
			},
			toString: func() string {
				return class_name
			},
		},
	}
}
