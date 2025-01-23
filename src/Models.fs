module Models
    type Token =
        //special types
        | ILLEGAL
        | EOF
        //identifiers + literals
        | NUMBER
        //operators
        | PLUS
        //delimeters
        | PERIOD
        | NEW_LINE
        | SEMICOLON

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
