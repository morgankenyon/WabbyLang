﻿module Lexer
    open Models

    type ComplexTokenType =
        //| Letter
        | Digit
        | Illegal

    let readChar (l: LexerState) =
        let newChar =
            match l.readPosition >= l.input.Length with
            | true -> '\000'
            | false -> l.input.Chars l.readPosition
        l.position <- l.readPosition
        l.readPosition <- l.readPosition + 1
        l.ch <- newChar

    let isLetter(ch: char) =
        let lowerCase = ch.CompareTo('a') >= 0 && ch.CompareTo('z') <= 0
        let upperCase = ch.CompareTo('A') >= 0 && ch.CompareTo('Z') <= 0;
        let underscore = ('_' = ch);
        lowerCase || upperCase || underscore

    let isDigit(ch: char) =
        ch.CompareTo('0') >= 0 && ch.CompareTo('9') <= 0

    let canReadDigit(l: LexerState) =
        //ensure I can read next position
        let canReadNextPosition = l.position + 1 < l.input.Length
        canReadNextPosition && isDigit(l.input.Chars(l.position + 1)) 

    let readNumber(l: LexerState) =
        let pos = l.position
        while canReadDigit(l) do 
            readChar l
        let literal = l.input.Substring(pos, (l.position - pos + 1))
        (NUMBER, literal)

    let findComplexTokenType l =
        //if isLetter(l.ch) then
        //    Letter
        if isDigit(l.ch) then
            Digit
        else
            Illegal

    let nextComplexToken(l: LexerState) =
        match findComplexTokenType(l) with 
        //| Letter -> readIdentifier(l)
        | Digit -> readNumber(l)
        | Illegal -> (Token.ILLEGAL, l.ch.ToString())

    let skipWhitespace(l: LexerState) =
        while l.ch = ' ' || l.ch = '\t' || l.ch = '\n' || l.ch = '\r' do
            readChar l
        ()
    let nextToken (l: LexerState) =
        skipWhitespace l

        let (tokenType, literal) =
            match l.ch with
            | '\000' -> (Token.EOF, "")
            | '+' -> (Token.PLUS, l.ch.ToString())
            | _ -> nextComplexToken l

        let token = { Token = tokenType; Literal = literal }
        
        readChar l
        token

    let createLexer input =
        let lexer = { input = input; position = 0; readPosition = 0; ch = '\000'}
        readChar lexer
        lexer
