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
    let toIl    = lex >> parse >> optimize >> codeGen