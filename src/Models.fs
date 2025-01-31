module Models

type Token =
    //special types
    | ILLEGAL
    | EOF
    | UNKNOWN
    //identifiers + literals
    | IDENT
    | NUMBER
    //operators
    | ASSIGN
    | PLUS
    | MINUS
    | ASTERISK
    | SLASH
    | EQ
    | NOT_EQ
    | LT
    | LT_EQ
    | GT
    | GT_EQ
    | AND
    | OR
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
    | WHILE

let internal TokenToStr (token : Token) = 
    match token with
    | EQ -> "=="
    | NOT_EQ -> "!="
    | LT -> "<"
    | LT_EQ -> "<="
    | GT -> ">"
    | GT_EQ -> ">="
    | _ -> ""
let internal StrToToken (value : string) =
    match value with
    | "==" -> EQ
    | "!=" -> NOT_EQ
    | "<" -> LT
    | "<=" -> LT_EQ
    | ">" -> GT
    | ">=" -> GT_EQ
    | "and" -> AND
    | "or" -> OR
    | "while" -> WHILE
    | _ -> UNKNOWN


type TokenPair = { Token: Token; Literal: string }

type LexerState =
    { input: string
      mutable position: int
      mutable readPosition: int
      mutable ch: char }
