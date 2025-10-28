package interpreter

var environment map[string]any

func initializeEnvironment() {
	environment = make(map[string]any, 0)
}
