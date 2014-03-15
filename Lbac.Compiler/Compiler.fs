module Compiler
    open System.Reflection
    open Lex
    open Syntax
    open IL
    open CodeGenerator
    open Railway

    let private identity = fun arg -> arg
    let private lex = Lex.tokenize
    let private parse = Syntax.parse
    let private optimize = switch identity
    let private codeGen = CodeGenerator.codegen
    let private methodBuilder = switch(IL.toMethod typedefof<System.Int32>)

    let compile = lex >> parse >=> optimize >=> codeGen >=> methodBuilder
    let toIl    = lex >> parse >=> optimize >=> codeGen