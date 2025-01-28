module Models
    type Token =
        //special types
        | ILLEGAL
        | EOF
        //identifiers + literals
        | IDENT
        | NUMBER
        //operators
        | ASSIGN
        | PLUS
        | MINUS
        | ASTERISK
        | SLASH
        //delimeters
        //| PERIOD
        | COMMA
        | NEW_LINE
        | SEMICOLON
        | LPAREN
        | RPAREN
        | LBRACE
        | RBRACE
        //keywords
        | FUNC
        | LET
        | IF
        | ELSE
        | ELIF

    type TokenPair =
        {
            Token : Token
            Literal : string
        }

    type LexerState =
        {
            input : string
            mutable position : int
            mutable readPosition : int
            mutable ch : char
        }
