package interpreter

type LoxClass struct {
	name     string
	toString func() string
}
