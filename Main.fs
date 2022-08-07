open System.IO
open FSharp.Text

[<EntryPoint>]
let main argv =
  let code = "example.mnk" |> File.ReadAllText

  let lexbuf = Lexing.LexBuffer<_>.FromString code
  let res = Parser.StatList Lexer.tokenize lexbuf

  let v = Eval.eval res
  printfn "RES: %A" v
  0
