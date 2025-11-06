package parser

import (
	"errors"

	"github.com/codecrafters-io/interpreter-starter-go/app/loxerrors"
	//lint:ignore ST1001 I dont care
	. "github.com/codecrafters-io/interpreter-starter-go/app/token"
)

type NodeType string

const (
	// expression types
	BINARY     NodeType = "binary"
	UNARY      NodeType = "unary"
	TERMINAL   NodeType = "terminal"
	STRINGNODE NodeType = "string"
	NUMBERNODE NodeType = "number"
	GROUP      NodeType = "group"
	VARIABLE   NodeType = "variable"
	ASSIGNMENT NodeType = "assignment"
	LOGICALOP  NodeType = "logical_operator"
	// statement types
	BLOCK     NodeType = "block"
	PRINTSTM  NodeType = "print_statement"
	EXPRSTM   NodeType = "expression_statement"
	VARDECLR  NodeType = "variable_declaration"
	IFSTMT    NodeType = "if_statement"
	WHILESTMT NodeType = "while_statement"
)

// Abstract Syntax Tree Node
type AstNode struct {
	Representation any
	Type           NodeType
	Children       []*AstNode
}

/*  Context Free Grammer from low to high precedence of resolution:
// Program level
program        → declaration* EOF ;

declaration    → varDecl
			   | statement ;

statement      → exprStmt
			   | forStmt
               | ifStmt
			   | whileStmt
               | printStmt
               | block ;

forStmt        → "for" "(" ( varDecl | exprStmt | ";" )
                 expression? ";"
                 expression? ")" statement ;

whileStmt      → "while" "(" expression ")" statement ;

ifStmt         → "if" "(" expression ")" statement
               ( "else" statement )? ;

block		   → "{" declaration* "}"

exprStmt       → expression ";" ;
printStmt      → "print" expression ";" ;

// Expression level
expression     → assignment ;
assignment     → IDENTIFIER "=" assignment
			   | logic_or ;
logic_or       → logic_and ( "or" logic_and )* ;
logic_and      → equality ( "and" equality )* ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary
               | primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" | IDENTIFIER ;
*/

func declaration() (*AstNode, error) {
	var ast_node *AstNode
	var err error
	if match(VAR) {
		ast_node, err = varDeclaration()
	} else {
		ast_node, err = statement()
	}

	if err != nil {
		synchronize()
		return nil, err
	} else if *loxerrors.GetRuntimeErrorState() {
		synchronize()
		return nil, nil
	}

	return ast_node, err
}

func varDeclaration() (*AstNode, error) {
	name, err := consume(IDENTIFIER, "Expect variable name.")
	if err != nil {
		return nil, err
	}

	var initializer *AstNode = &AstNode{ // nil equivalent node
		Representation: Token{
			Type:    NIL,
			Lexeme:  "",
			Literal: nil,
			Line:    name.Line,
		},
		Type:     TERMINAL,
		Children: nil,
	}

	if match(EQUAL) {
		initializer, err = expression()
		if err != nil {
			return nil, err
		}
	}

	_, err = consume(SEMICOLON, "Expect ';' after variable declaration")
	if err != nil {
		return nil, err
	}

	return &AstNode{
		Representation: name.Lexeme,
		Type:           VARDECLR,
		Children:       []*AstNode{initializer},
	}, nil

}

func statement() (*AstNode, error) {
	if match(PRINT) {
		return genericStatement(true)
	} else if match(FOR) {
		return forStatement()
	} else if match(IF) {
		return ifStatement()
	} else if match(WHILE) {
		return whileStatement()
	} else if match(LEFT_BRACE) {
		return block()
	}

	return genericStatement(false)
}

func forStatement() (*AstNode, error) {
	_, err := consume(LEFT_PAREN, "Expect left parenthesis.")
	if err != nil {
		return nil, err
	}

	// initializer
	var initializer *AstNode
	if match(SEMICOLON) {
		initializer = nil
	} else if match(VAR) {
		initializer, err = varDeclaration() // declaration will consume semicolon
		if err != nil {
			return nil, err
		}
	} else {
		initializer, err = genericStatement(false) // expression statement will consume semicolon
		if err != nil {
			return nil, err
		}
	}

	// condition
	var condition *AstNode = &AstNode{ // if no condition, it is true by default
		Representation: Token{
			Type:    TRUE,
			Lexeme:  "",
			Literal: true,
			Line:    global_tokens[current_token].Line,
		},
		Type:     TERMINAL,
		Children: nil,
	}

	if peek().Type != SEMICOLON {
		condition, err = expression()
		if err != nil {
			return nil, err
		}
	}

	consume(SEMICOLON, "Expect ';' after loop condition.")

	// increment
	var increment *AstNode = nil
	if peek().Type != RIGHT_PAREN {
		increment, err = expression()
		if err != nil {
			return nil, err
		}
	}

	_, err = consume(RIGHT_PAREN, "Expect right parenthesis.")
	if err != nil {
		return nil, err
	}

	// loop body
	body, err := statement()
	if err != nil {
		return nil, err
	}

	// transform or desugar to a while statement

	// generate while body
	children := []*AstNode{body}
	if increment != nil {
		children = append(children, increment)
	}
	while_block := &AstNode{
		Representation: nil,
		Type:           BLOCK,
		Children:       children,
	}

	// generate core while statement
	while := &AstNode{
		Representation: nil,
		Type:           WHILESTMT,
		Children:       []*AstNode{condition, while_block},
	}

	// generate block scope so variable declaration will go out of scope after for statement
	children = []*AstNode{}
	if initializer != nil {
		children = append(children, initializer)
	}
	children = append(children, while)

	return &AstNode{
		Representation: nil,
		Type:           BLOCK,
		Children:       children,
	}, nil
}

func ifStatement() (*AstNode, error) {
	_, err := consume(LEFT_PAREN, "Expect left parenthesis")
	if err != nil {
		return nil, err
	}

	condition, err := expression()
	if err != nil {
		return nil, err
	}

	_, err = consume(RIGHT_PAREN, "Expect right parenthesis")
	if err != nil {
		return nil, err
	}

	then_statement, err := statement()
	if err != nil {
		return nil, err
	}

	var else_statement *AstNode = nil
	if match(ELSE) {
		else_statement, err = statement()
		if err != nil {
			return nil, err
		}
	}

	return &AstNode{
		Representation: nil,
		Type:           IFSTMT,
		Children:       []*AstNode{condition, then_statement, else_statement},
	}, nil
}

func whileStatement() (*AstNode, error) {
	_, err := consume(LEFT_PAREN, "Expect left parenthesis")
	if err != nil {
		return nil, err
	}

	condition, err := expression()
	if err != nil {
		return nil, err
	}

	_, err = consume(RIGHT_PAREN, "Expect right parenthesis")
	if err != nil {
		return nil, err
	}

	statement, err := statement()
	if err != nil {
		return nil, err
	}

	return &AstNode{
		Representation: nil,
		Type:           WHILESTMT,
		Children:       []*AstNode{condition, statement},
	}, nil
}

func block() (*AstNode, error) {
	var statements []*AstNode

	for peek().Type != RIGHT_BRACE && peek().Type != EOF {
		stm, err := declaration()
		if err != nil {
			return nil, err
		}
		statements = append(statements, stm)
	}

	consume(RIGHT_BRACE, "Expect '}' at the end of block.")

	return &AstNode{
		Representation: nil,
		Type:           BLOCK,
		Children:       statements,
	}, nil
}

func genericStatement(is_print bool) (*AstNode, error) {
	var node_type NodeType
	if is_print {
		node_type = PRINTSTM
	} else {
		node_type = EXPRSTM
	}

	expr_value, err := expression()
	if err != nil {
		return nil, err
	}

	consume(SEMICOLON, "Expect ';' after value.")

	return &AstNode{
		Representation: nil,
		Type:           node_type,
		Children:       []*AstNode{expr_value},
	}, nil

}

func expression() (*AstNode, error) {
	return assignment()
}

func assignment() (*AstNode, error) {
	// we evaluate it like an expression and if there is no assignment operator, we return this
	// if there is a assignment operator, we check if left side can be assigned to and
	//  we just save the token to later assign the r_value
	l_value, err := logicOr()
	if err != nil {
		return nil, err
	}

	if match(EQUAL) {
		equal_token := previous()
		r_value, err := assignment()
		if err != nil {
			return nil, err
		}

		if l_value.Type != VARIABLE {
			// if l_value is not something we can assign to
			err = loxerrors.RuntimeError(equal_token, "Invalid assignment target.")
			return nil, err
		}

		return &AstNode{
			Representation: l_value.Representation,
			Type:           ASSIGNMENT,
			Children:       []*AstNode{r_value},
		}, nil
	}

	return l_value, nil
}

func logicOr() (*AstNode, error) {
	left, err := logicAnd()
	if err != nil {
		return nil, err
	}

	for match(OR) {
		operator := previous()
		right, err := logicAnd()
		if err != nil {
			return nil, err
		}

		left = &AstNode{
			Representation: operator,
			Type:           LOGICALOP,
			Children:       []*AstNode{left, right},
		}
	}

	return left, nil
}

func logicAnd() (*AstNode, error) {
	left, err := equality()
	if err != nil {
		return nil, err
	}

	for match(AND) {
		operator := previous()
		right, err := equality()
		if err != nil {
			return nil, err
		}

		left = &AstNode{
			Representation: operator,
			Type:           LOGICALOP,
			Children:       []*AstNode{left, right},
		}
	}

	return left, nil
}

func equality() (*AstNode, error) {
	var expr *AstNode
	expr, err := comparison()
	if err != nil {
		return nil, err
	}

	for match(BANG_EQUAL, EQUAL_EQUAL) {
		// further recursion calls will move pointer so save operator
		operator := previous()

		right, err := comparison()
		if err != nil {
			return nil, err
		}

		expr = &AstNode{
			Representation: operator,
			Type:           BINARY,
			Children:       []*AstNode{expr, right},
		}
	}

	return expr, nil
}

func comparison() (*AstNode, error) {
	var expr *AstNode
	expr, err := term()
	if err != nil {
		return nil, err
	}

	for match(LESS, LESS_EQUAL, GREATER, GREATER_EQUAL) {
		// further calls will move pointer so save the bang or minus operator
		operator := previous()

		right, err := term()
		if err != nil {
			return nil, err
		}

		expr = &AstNode{
			Representation: operator,
			Type:           BINARY,
			Children:       []*AstNode{expr, right},
		}
	}

	return expr, nil
}

func term() (*AstNode, error) {
	var expr *AstNode
	expr, err := factor()
	if err != nil {
		return nil, err
	}

	for match(MINUS, PLUS) {
		// further recursion calls will move pointer so save operator
		operator := previous()

		right, err := factor()
		if err != nil {
			return nil, err
		}

		expr = &AstNode{
			Representation: operator,
			Type:           BINARY,
			Children:       []*AstNode{expr, right},
		}
	}

	return expr, nil
}

func factor() (*AstNode, error) {
	var expr *AstNode
	expr, err := unary()
	if err != nil {
		return nil, err
	}

	for match(STAR, SLASH) {
		// further recursion calls will move pointer so save operator
		operator := previous()

		right, err := unary()
		if err != nil {
			return nil, err
		}

		expr = &AstNode{
			Representation: operator,
			Type:           BINARY,
			Children:       []*AstNode{expr, right},
		}
	}

	return expr, nil
}

func unary() (*AstNode, error) {
	if match(BANG, MINUS) {
		// further unary calls will move pointer so save the bang or minus operator
		operator := previous()

		child, err := unary()
		if err != nil {
			return nil, err
		}

		expr := &AstNode{
			Representation: operator,
			Type:           UNARY,
			Children:       []*AstNode{child},
		}

		return expr, nil
	}

	return primary()
}

func primary() (*AstNode, error) {
	if match(TRUE) {
		return &AstNode{
			Representation: previous(),
			Type:           TERMINAL,
			Children:       nil,
		}, nil
	}

	if match(FALSE) {
		return &AstNode{
			Representation: previous(),
			Type:           TERMINAL,
			Children:       nil,
		}, nil
	}

	if match(NIL) {
		return &AstNode{
			Representation: previous(),
			Type:           TERMINAL,
			Children:       nil,
		}, nil
	}

	if match(IDENTIFIER) {
		return &AstNode{
			Representation: previous(),
			Type:           VARIABLE,
			Children:       nil,
		}, nil
	}

	if match(STRING) {
		return &AstNode{
			Representation: previous().Literal,
			Type:           STRINGNODE,
			Children:       nil,
		}, nil
	}

	if match(NUMBER) {
		return &AstNode{
			Representation: previous().Literal,
			Type:           NUMBERNODE,
			Children:       nil,
		}, nil
	}

	if match(LEFT_PAREN) {
		expr, err := expression()
		if err != nil {
			return nil, err
		}

		expr = &AstNode{
			Representation: "group",
			Type:           GROUP,
			Children:       []*AstNode{expr},
		}

		_, err = consume(RIGHT_PAREN, "Expected ')' after expression.")

		return expr, err
	}

	loxerrors.ParserError(peek(), "Expect expression.")
	return nil, errors.New(`Unidentified token: ` + peek().Lexeme)

}
