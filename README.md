# Golox

This repo builds an interpreter in Golang following the book
[Crafting Interpreters](https://craftinginterpreters.com/) by Robert Nystrom, implementing the spec for [Lox](https://craftinginterpreters.com/the-lox-language.html), a simple scripting language.

This solution passes all tests in the comprehensive test suite in
["Build your own Interpreter" Challenge](https://app.codecrafters.io/courses/interpreter/overview).

Along the way, you'll learn about tokenization, ASTs,
tree-walk interpreters and more.

## Usage

- Ensure you have `go (1.24)` or above installed locally
- Navigate to the root folder of this project.

### Linux / MacOS

- To quickly build and run the interpreter run `./golox.sh` to run your program, which is implemented in `app/main.go`.

```bash
./golox.sh run lox_examples/test_class.lox
```

- To manually build and run the interpreter

```bash
go build -o /build/golox app/*.go
cd build
```

```bash
golox run lox_examples/test_class.lox
```
