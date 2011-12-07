module Lbac.Program 
    
open System
open System.IO

let main() = 
    let outputStream = Console.Out
    let keyReader() = 
       let cki = Console.ReadKey()
       cki.KeyChar
    let cradle = new Lbac.ExpressionParsing(keyReader, outputStream)
    ignore cradle.expression
    ignore( Console.ReadLine() )

main()