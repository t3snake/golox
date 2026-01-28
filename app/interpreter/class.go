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

// Get property of Lox Instance. Searches token if fields first and then methods. If not found reports an error.
func (inst *LoxInstance) get(property Token, env *EnvironmentNode) (any, error) {
	// search properties or fields of class
	field_val, ok := inst.fields[property.Lexeme]
	if ok {
		return field_val, nil
	}

	// search method of the class
	method, ok := inst.class.methods[property.Lexeme]
	if ok && env != nil {
		return injectThisIntoMethod(*inst, method, env), nil
	}
	return nil, loxerrors.RuntimeError(property,
		fmt.Sprintf("Undefined property %s.", property.Lexeme))
}

// Sets a property or method to the lox instance
func (inst *LoxInstance) set(property Token, value any) {
	inst.fields[property.Lexeme] = value
}

// Injects a new environment with a binding for 'this' to the instance on which the method is called.
func injectThisIntoMethod(inst LoxInstance, method LoxFunction, env *EnvironmentNode) *LoxFunction {
	new_env := initializeEnvironment(env)
	new_env.bindings["this"] = inst
	modified_method := constructLoxFunction(method.Lexeme, method.Parameters, method.Block, new_env, method.IsInitializer)
	return modified_method
}

func constructLoxClass(class_name string, methods map[string]LoxFunction, env *EnvironmentNode) *LoxClass {
	class := &LoxClass{
		name:    class_name,
		methods: methods,
	}

	// arity is 0 if no explicit constructor
	arity := 0
	init_meth, isInitAvailable := methods["init"]
	if isInitAvailable {
		// if init available, arity is equal to number of arguments
		arity = init_meth.arity
	}

	class.LoxFunction = LoxFunction{
		Lexeme: class_name,
		arity:  arity,
		call: func(arguments []any) any { // this call is the constructor call
			instance := LoxInstance{
				class:  class,
				fields: make(map[string]any),
				toString: func() string {
					return fmt.Sprintf("%s instance", class_name)
				},
			}

			if isInitAvailable {
				modified_meth := injectThisIntoMethod(instance, init_meth, env)
				modified_meth.call(arguments)
			}

			return instance
		},
		toString: func() string {
			return class_name
		},
	}

	return class
}
