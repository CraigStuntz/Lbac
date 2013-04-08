module Compiler
    open System.Reflection
    open Lex
    open Syntax
    open IL

    type Lexer = string -> seq<Token>
    type Parser = seq<Token> -> Expr
    type CodeGenerator = Expr -> seq<instruction>
    type AssemblyEmitter = seq<instruction> -> Assembly
    type Compiler = string -> Assembly
