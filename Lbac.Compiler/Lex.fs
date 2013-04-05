module Lex

    type Token =
        | Number of int
        | Symbol of char

    type Lexer = string -> seq<Token>
