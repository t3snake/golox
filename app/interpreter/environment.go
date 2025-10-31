package interpreter

type EnvironmentNode struct {
	environment map[string]any
	parent      *EnvironmentNode
}

func initializeEnvironment(parent *EnvironmentNode) *EnvironmentNode {
	return &EnvironmentNode{
		environment: make(map[string]any, 0),
		parent:      parent,
	}
}

// and return val if key in environment or their parents
func getValueIfKeyInEnvironment(key string, node *EnvironmentNode) (*EnvironmentNode, any) {
	travel_node := node
	for travel_node != nil {
		val, ok := travel_node.environment[key]
		if ok {
			return travel_node, val
		}

		travel_node = travel_node.parent
	}

	return nil, nil
}

func assignValueIfKeyExists(key string, value any, node *EnvironmentNode, is_var_declr bool) (exists bool) {
	target_node, _ := getValueIfKeyInEnvironment(key, node)
	if (target_node == nil && is_var_declr) || target_node != nil {
		node.environment[key] = value
		return true
	}

	return false
}
