package interpreter

import "github.com/codecrafters-io/interpreter-starter-go/app/parser"

type EnvironmentNode struct {
	bindings map[string]any
	parent   *EnvironmentNode
}

// Global Environment that stores variables in global state.
var globalEnvironment *EnvironmentNode

// Local Scope that stores resolutions of variables and the depth at which they resolve, which is calculated during the resolver step.
var local_scope map[*parser.AstNode]int

// Exposes resolve method which allows resolver step to store resolution information for subsequent use in intepreter.
func Resolve(expr_node *parser.AstNode, depth int) {
	local_scope[expr_node] = depth
}

func initializeEnvironment(parent *EnvironmentNode) *EnvironmentNode {
	return &EnvironmentNode{
		bindings: make(map[string]any, 0),
		parent:   parent,
	}
}

// and return val if key in environment or their parents
func getValueIfKeyInEnvironment(key string, node *EnvironmentNode) (*EnvironmentNode, any) {
	travel_node := node
	for travel_node != nil {
		val, ok := travel_node.bindings[key]
		if ok {
			return travel_node, val
		}

		travel_node = travel_node.parent
	}

	return nil, nil
}

func assignValueIfKeyExists(key string, value any, node *EnvironmentNode, is_var_declr bool) (exists bool) {
	if is_var_declr {
		// add key if var declaration in current environment
		node.bindings[key] = value
		return true
	}

	target_node, _ := getValueIfKeyInEnvironment(key, node)
	if target_node != nil {
		// if key found reassign in the environment where key was found
		target_node.bindings[key] = value
		return true
	}

	return false
}
