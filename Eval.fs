module Eval

open FSharp.Core
open Ast

type VarMap    = Map<string, Expr>
type CallStack = VarMap list

let rec getn (n: int) (l: Expr list): Expr option =
  match n, l with
  | 0, (x :: _)   -> Some( x )
  | _, (_ :: xs ) -> getn (n - 1) xs
  | _, []         -> None

let lookupIdent state s : Expr option =
  Map.tryFind s state

let wrapLogical x =
  match x with
  | true -> True
  | false -> False

let splitlist (x: CallStack) =
  match x with
  | [] -> Map.empty, []
  | a :: b -> a, b

let cstate (x: CallStack) =
  let r, _ = splitlist x
  r

let crest (x: CallStack) =
  let _, r = splitlist x
  r

module intNode =

  let evalMath op (a: int) (b: int) =
    match op with
    | Add -> Expr.Integer( a + b )
    | Sub -> Expr.Integer( a - b )
    | Mul -> Expr.Integer( a * b )
    | Div -> Expr.Integer( a / b )
    | Mod -> Expr.Integer( a % b )
    | _ -> ErrorMsg( sprintf "The following operator %A is not supported!" op )


  let evalLogical op (a: int) (b: int) =
    match op with
    | Equal          -> wrapLogical( a = b )  |> BoolValue |> Primitive
    | NotEqual       -> wrapLogical( a <> b ) |> BoolValue |> Primitive
    | LessThan       -> wrapLogical( a < b )  |> BoolValue |> Primitive
    | GreaterThan    -> wrapLogical( a > b )  |> BoolValue |> Primitive
    | LessOrEqual    -> wrapLogical( a <= b ) |> BoolValue |> Primitive
    | GreaterOrEqual -> wrapLogical( a >= b ) |> BoolValue |> Primitive
    | _ -> ErrorMsg( sprintf "The following operator %A is not supported!" op )

  let eval op (a: int) (b: int): Expr =
    match op with
    | Add
    | Sub
    | Mul
    | Div
    | Mod -> evalMath op a b

    | Equal
    | NotEqual
    | LessThan
    | GreaterThan
    | LessOrEqual
    | GreaterOrEqual -> evalLogical op a b

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
      let curframe = cstate callstack
      let res_a = lookupIdent curframe ia
      let res_b = lookupIdent curframe ib
      printfn "DEBUG: %A %A" res_a res_b
      callstack, ErrorMsg( sprintf "The types of %A and %A are not compatible!" ea eb )
  | _ -> callstack, ErrorMsg( sprintf "The types of %A and %A are not compatible!" ea eb )

and evalUnary callstack op e =
  failwith "evalUnary is not implemented yet!"

and evalExpr (callstack: CallStack) (exp: Expr) : (CallStack * Expr) =
  let curFrame, restOfStack = splitlist callstack
  match exp with
  | (Primitive x) -> callstack, exp
  | (Integer i) -> callstack, exp
  | (String s)  -> callstack, exp
  | (Identifier s) ->
    match (lookupIdent curFrame s) with
      | Some(res) -> callstack, res 
      | None      -> restOfStack, ErrorMsg( sprintf "No value is bound to %A" s )
  | Array(_)    -> callstack, exp
  | IndexAccess(x, arrexpr) ->
      let newcallstack, arrexpr = evalExpr callstack arrexpr
      let arr =
        match arrexpr with
        | Array(x) -> x
        | _        -> []

      let resval = 
        match x with
        | Integer(i) ->
            let res = getn i arr
            match res with
            | Some( arrval ) -> arrval
            | None           -> ErrorMsg( sprintf "The index beyond the size of array! %A" arr)
        | _ -> ErrorMsg( sprintf "The index access can be done via integers only! Got %A instead" x)
      newcallstack, resval
  | Binary( a, op, b ) -> evalBinary callstack op a b
  | Unary( op, e )     -> evalUnary callstack op e
  | Function(_, _, _)  -> callstack, exp
  | FuncDef( args, body ) -> callstack, Function( args, body, curFrame )
  | FuncCall( (Identifier funcname), args ) ->
      let temp = lookupIdent curFrame funcname
      match temp with
      | Some(Function(argnames, funcbody, env)) ->
          let newstate = matchargs argnames args env
          let newstack = newstate :: callstack
          funcbody |> List.fold (fun (curstate, _) s -> evalNode curstate s ) (newstack, Unit)
      | Some(_) -> restOfStack, ErrorMsg( sprintf "%A is not a function!" funcname )
      | None    -> restOfStack, ErrorMsg( sprintf "function %A doesn't exist!" funcname )
  | FuncCall(_,_) -> restOfStack, ErrorMsg( "Misconstructed function call!" )
  | Unit | Error | ErrorMsg(_) -> restOfStack, exp


and evalNode (callstack: CallStack) (node: Stat) : ( CallStack * Expr ) =
  match node with
  | Expr(e) -> evalExpr callstack e
  | Let(e)  -> 
      match e with
      | Binary (id, Assign, rval) -> 
          match id with
          | Identifier(name) ->
              let newstack, rval = evalExpr callstack rval
              let vars, rest = splitlist newstack
              let new_state = Map.add name rval vars
              let newstack = new_state :: rest
              newstack, Unit
          | _ -> 
              let reststack = crest callstack
              reststack, ErrorMsg "The lhs can only be lval!"
      | _ -> 
          let rest = crest callstack
          rest, ErrorMsg "Misconstrued led statment!"
  | Return( Some e ) -> 
      let rest = crest callstack
      rest, e
  | Return( None )   -> 
      let rest = crest callstack
      rest, Unit
  | Block( stats ) ->
      let newstate = cstate callstack
      let newstack = newstate :: callstack
      foldStatements newstack stats
  | If( cond, trueBranch, falseBranch ) ->
      let _, res = evalExpr callstack cond
      match res with
      | Primitive(BoolValue True) -> foldStatements callstack trueBranch
      | Primitive(_)              -> foldStatements callstack falseBranch
      | _ -> callstack, ErrorMsg( sprintf "The %A does not return a boolean!" cond )

and foldStatements (state: CallStack) (stats: Stat list) : ( CallStack * Expr ) =
  stats |> List.fold (fun (curstate, _) s -> (evalNode curstate s) ) (state, Unit)

let eval (lst: Stat list) =
  let (_, res) = foldStatements [ Map.empty ] lst
  res
