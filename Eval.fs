module Eval

open FSharp.Core
open Ast

type VarMap    = Map<string, Expr>
type CallStack = List<VarMap>

let lookupIdent state s : Expr option =
  Map.tryFind s state

module intNode =

  let eval op (a: int) (b: int) =
    match op with
    | Add -> Expr.Integer( a + b )
    | Sub -> Expr.Integer( a - b )
    | Mul -> Expr.Integer( a * b )
    | Div -> Expr.Integer( a / b )
    | Mod -> Expr.Integer( a % b )
    | _ -> ErrorMsg( sprintf "The following operator %A is not supported!" op )

let rec matchargs argnames args state =
  let foo x =
    match x with
    | (Identifier name) -> name
    | _ -> "<empty>"
  let newstack = [state]
  let bar x = 
    let _, res = evalExpr newstack x
    res
  let argnames = List.map foo argnames
  let args     = List.map bar args
  List.zip argnames args |> List.fold (fun acc (k, v) -> Map.add k v acc ) state

and evalBinary callstack op a b =
  let (_, ea), (_, eb) = (evalExpr callstack a), (evalExpr callstack b)
  match ea, eb with
  | (Integer ia), (Integer ib) -> callstack, intNode.eval op ia ib
  | (Identifier ia), (Identifier ib) ->
      let curframe :: _ = callstack
      let res_a = lookupIdent curframe ia
      let res_b = lookupIdent curframe ib
      printfn "DEBUG: %A %A" res_a res_b
      callstack, ErrorMsg( sprintf "The types of %A and %A are not compatible!" ea eb )
  | _ -> callstack, ErrorMsg( sprintf "The types of %A and %A are not compatible!" ea eb )

and evalUnary callstack op e =
  failwith "evalUnary is not implemented yet!"

and evalExpr (callstack: CallStack) (exp: Expr) : (CallStack * Expr) =
  let curFrame :: restOfStack = callstack
  match exp with
  | (Integer i) -> callstack, exp
  | (Identifier s) ->
    match (lookupIdent curFrame s) with
      | Some(res) -> callstack, res 
      | None      -> restOfStack, ErrorMsg( sprintf "No value is bound to %A" s )
  | Expr.Binary( a, op, b ) -> evalBinary callstack op a b
  | Expr.Unary( op, e )     -> evalUnary callstack op e
  | Expr.Function( args, body ) -> callstack, exp
  | Expr.FuncCall( (Identifier funcname), args ) ->
      let temp = lookupIdent curFrame funcname
      match temp with
      | Some(Function(argnames, funcbody)) ->
          let newstate = matchargs argnames args curFrame
          let newstack = newstate :: callstack
          funcbody |> List.fold (fun (curstate, _) s -> evalNode (curstate, s) ) (newstack, Unit)
      | Some(_) -> restOfStack, ErrorMsg( sprintf "%A is not a function!" funcname )
      | None -> restOfStack, ErrorMsg( sprintf "function %A doesn't exist!" funcname )
  | Expr.FuncCall(_,_) -> restOfStack, Expr.ErrorMsg( "Misconstructed function call!" )
  | Expr.Unit | Expr.Error | Expr.ErrorMsg(_) -> restOfStack, exp


and evalNode (callstack: CallStack, node: Stat) : ( CallStack * Expr ) =
  match node with
  | Stat.Expr(e) -> evalExpr callstack e
  | Stat.Let(e)  -> 
      match e with
      | Binary (id, Assign, rval) -> 
          match id with
          | Identifier(name) ->
              let newstack, rval = evalExpr callstack rval
              let vars :: rest = newstack
              let new_state = Map.add name rval vars
              let newstack = new_state :: rest
              newstack, Unit
          | _ -> 
              let _ :: reststack = callstack
              reststack, ErrorMsg "The lhs can only be lval!"
      | _ -> 
          let s :: rest = callstack
          rest, ErrorMsg "Misconstrued led statment!"
  | Stat.Return( Some e ) -> 
      let s :: rest = callstack
      rest, e
  | Stat.Return( None )   -> 
      let _ :: rest = callstack
      rest, Unit

let foldStatements (state: CallStack) (stats: Stat list) : ( CallStack * Expr ) =
  stats |> List.fold (fun (curstate, _) s -> evalNode (curstate, s) ) (state, Unit)

let eval lst =
  let curstate, res = foldStatements [ Map.empty ] lst
  res
