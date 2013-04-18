module Compiler
    open System.Reflection
    open Lex
    open Syntax
    open IL
    open CodeGenerator

    let private lex = Lex.tokenize
    let private parse = Syntax.parse
    let private optimize ast = ast
    let private codeGen = CodeGenerator.codegen
    let private methodBuilder il = IL.toMethod(il, typedefof<System.Int32>)

    let compile = lex >> parse >> optimize >> codeGen >> methodBuilder

    type Lexer = string -> seq<Token>
    type Parser = seq<Token> -> Expr
    type Optimizer = Expr -> Expr
    type CodeGenerator = Expr -> seq<instruction>
    type AssemblyEmitter = seq<instruction> -> Assembly
    type Compiler = string -> Assembly
