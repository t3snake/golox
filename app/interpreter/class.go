package interpreter

import (
	"fmt"
)

type LoxClass struct {
	name        string
	LoxFunction // embed LoxFunction struct
}

type LoxInstance struct {
	toString func() string
}

func constructLoxClass(class_name string) *LoxClass {
	return &LoxClass{
		name: class_name,
		LoxFunction: LoxFunction{
			Lexeme: class_name,
			arity:  0,
			call: func(arguments []any) any {
				return LoxInstance{
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
