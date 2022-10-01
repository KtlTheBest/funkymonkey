module Eval

open FSharp.Core
open Ast

type VarMap       = Map<string, Expr>
type EvalRes      = VarMap * DataType

type Either<'a, 'b> =
  | Left of 'a
  | Right of 'b

let wraperr1 msg x   = sprintf msg x   |> ErrorMsg |> Left
let wraperr2 msg x y = sprintf msg x y |> ErrorMsg |> Left

let (>>=) x f =
  match x with
  | Left l -> Left l
  | Right r -> f r

let extractTheDataType x =
  match x with
  | Expr(DataType(v)) -> Right (DataType v)
  | _ -> sprintf "Expected a DataType, but got %A" x |> ErrorMsg |> Left

let extractDataType x: Either<DataType, DataType> =
  match x with
  | DataType x -> Right x
  | Application y -> wraperr1 "Expected to have a DataType, but instead got %A" y

let extractInt x =
  match x with
  | Integer(i) -> Right i
  | _ -> wraperr1 "Expected int for index, but got %A!" x

let extractList x =
  match x with
  | Array(l) -> Right l
  | _ -> wraperr1 "Expected an array, but got %A!" x

let errormsg = ErrorMsg >> DataType

let rec getn (n: int) (l: Expr list): Expr option =
  match n, l with
  | 0, (x :: _)   -> Some( x )
  | _, (_ :: xs ) -> getn (n - 1) xs
  | _, []         -> None

let lookupIdent state s : Expr option =
  Map.tryFind s state

module intNode =

  let evalMath op (a: int) (b: int) =
    let wrap x = x |> Integer
    match op with
    | Add -> wrap <| a + b
    | Sub -> wrap <| a - b
    | Mul -> wrap <| a * b
    | Div -> wrap <| a / b
    | Mod -> wrap <| a % b
    | _ -> ErrorMsg <| sprintf "The following operator %A is not supported!" op


  let evalLogical op (a: int) (b: int) =
    let wrap (x: bool) = x |> Boolean
    match op with
    | Equal          -> wrap <| (a = b)
    | NotEqual       -> wrap <| (a <> b)
    | LessThan       -> wrap <| (a < b)
    | GreaterThan    -> wrap <| (a > b)
    | LessOrEqual    -> wrap <| (a <= b)
    | GreaterOrEqual -> wrap <| (a >= b)
    | _ -> ErrorMsg <| sprintf "The following operator %A is not supported!" op

  let eval op (a: int) (b: int) =
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

    | _ -> ErrorMsg <| sprintf "The following operator %A is not supported!" op

module stringNode =
  let evalLogical op (a: string) (b: string) =
    let wrap = Boolean

    match op with
    | Equal          -> wrap <| (a = b)
    | NotEqual       -> wrap <| (a <> b)
    | LessThan       -> wrap <| (a < b)
    | GreaterThan    -> wrap <| (a > b)
    | LessOrEqual    -> wrap <| (a <= b)
    | GreaterOrEqual -> wrap <| (a >= b)

  let eval op (a: string) (b: string) =
    match op with
    | Add      -> String  <| (a + b)
    | Equal    -> Boolean <| (a = b)
    | NotEqual -> Boolean <| (a <> b)

    | Sub
    | Mul
    | Div
    | Mod
    | _ -> ErrorMsg <| sprintf "The following operator %A is not supported for strings!" op

module boolNode =
  let eval op (a: bool) (b: bool) =
    match op with
    | Equal      -> Boolean <| (a = b)
    | NotEqual   -> Boolean <| (a <> b)
    | LogicalAnd -> Boolean <| (a && b)
    | LogicalOr  -> Boolean <| (a || b)

    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | LessThan
    | GreaterThan
    | LessOrEqual
    | GreaterOrEqual
    | _ -> ErrorMsg <| sprintf "The following operator %A is not supported for strings!" op

module arrayNode =
  let eval op (a: Expr list) (b: Expr list) =

    match op with
    | Add      -> [a; b] |> List.concat |> Array

    | Equal    -> Boolean <| (a = b)
    | NotEqual -> Boolean <| (a <> b)
    | _ -> ErrorMsg <| sprintf "The following operator %A is not supported for strings!" op

module unaryOp =

  let extractDataType e =
    match e with
    | DataType(v) -> Some v
    | Application(_) -> None

  let negateExpr v =
    match v with
    | Integer(i)     -> Integer <| -i
    | Null           -> Boolean <| true
    | Boolean(true)  -> Boolean <| false
    | Boolean(false) -> Boolean <| true

    | _ -> ErrorMsg <| sprintf "Negate opertor is defined only for integers and booleans! Instead got %A" v

  let negate x =
    let err = sprintf "Negate opertor is defined only for integers and booleans! Instead got %A" x
    x
    |> extractDataType
    |> Option.map negateExpr
    |> Option.defaultValue (err |> ErrorMsg)



let rec evalDataType state (dt: DataType) =
  let evaluator = evalExpr state
  let extract x =
    match x with
    | DataType(v) -> v
    | _ -> Null

  let res =
    match dt with
    | Array(lst) ->
        let res = lst |> List.map (fun x -> evaluator x)
        res |> DataType.Array
    | Identifier(id) ->
        state
        |> Map.tryFind id
        |> Option.defaultValue (DataType Null)
        |> extract

    | Null
    | Boolean(_)
    | Integer(_)
    | String (_)
    | Function(_, _, _)
    | ErrorMsg(_)      -> dt

  DataType <| res

and evalIndexAccess state index arr =
  let arr   = evalExpr state arr
  let index = evalExpr state index

  let processIndexAndArray i a =
    match getn i a with
    | Some x -> Right ( evalExpr state x )
    | None -> wraperr2 "The index %A is out of bounds on array %A" i a

  let evalres =
    extractDataType    index >>= fun di ->
    extractDataType      arr >>= fun da ->
    extractInt            di >>= fun i ->
    extractList           da >>= fun a ->
    processIndexAndArray i a >>= fun r ->
    extractDataType r

  match evalres with
  | Left x -> x
  | Right x -> x

and evalApplication env ap =

  let rec matchargs argnames args closure =
    let foo x =
      match x with
      | DataType(Identifier (name)) -> name
      | _ -> "<empty>"
    let bar = evalExpr closure
    let argnames = List.map foo argnames
    let args     = List.map bar args

    List.zip argnames args
    |> List.fold (fun acc (k, v) -> Map.add k v acc ) closure

  let res =
    match ap with
    | FuncDef(args, block) -> Function(args, block, env)
    | IndexAccess(index, arr) -> evalIndexAccess env index arr
    | FuncCall(f, args) ->
        match evalExpr env f with
        | DataType(Function(argsnames, block, fenv)) ->
            let closure = matchargs argsnames args fenv
            in
            evalStatements closure block |> snd
        | _ -> sprintf "Can only call functions! Instead got %A" f |> ErrorMsg

    | Binary(left, op, right) -> evalBinary env op left right
    | Unary(op, exp)          -> evalUnary env op exp
    | If( cond, trueBranch, falseBranch ) ->
        evalIf env trueBranch falseBranch cond

  res |> DataType

and evalBinary env op a b =
  let ea, eb = (evalExpr env a), (evalExpr env b)

  let validCombo da db =
    match da, db with
    | Integer(ia), Integer(ib) -> intNode   .eval op ia ib
    | String(sa),  String(sb)  -> stringNode.eval op sa sb
    | Boolean(ba), Boolean(bb) -> boolNode  .eval op ba bb
    | Array(aa),   Array(ab)   -> arrayNode .eval op aa ab
    | _ -> sprintf "The following operands' combo is not supported: %A %A" da db |> ErrorMsg

  match ea, eb with
  | DataType(da), DataType(db) -> validCombo da db
  | _ -> sprintf "The following operands' combo is not supported: %A %A" ea eb |> ErrorMsg

and evalUnary state op e =
  let v = evalExpr state e

  match op with
  | Sub  -> unaryOp.negate v
  | Bang -> unaryOp.negate v

and evalIf state truebranch falsebranch cond =
  let exp = evalExpr state cond

  let extractDataTypeFromExpr x =
    match x with
    | DataType v -> Some v
    | _ -> None

  let truthy x =
    match x with
    | Null                   -> false
    | Boolean(x)             -> x
    | Integer(i) when i = 0  -> false
    | Integer(i)             -> true
    | String(s)  when s = "" -> false
    | String(s)              -> true
    | Array(a)   when a = [] -> false
    | Array(a)               -> true
    | _                      -> true

  let evalif state truebranch falsebranch v =
    if truthy v then
      evalStatements state truebranch  |> snd
    else
      evalStatements state falsebranch |> snd

  let err =
    "Some error occured when trying to evaluate if-expression..."
    |> ErrorMsg

  exp
  |> extractDataTypeFromExpr
  |> Option.map (evalif state truebranch falsebranch)
  |> Option.defaultValue err

and evalExpr (state: VarMap) (exp: Expr) =
  match exp with
  | DataType   (dt) -> evalDataType    state dt
  | Application(ap) -> evalApplication state ap

and evalLet env var value =
  let v = evalExpr env value
  let newenv = env |> Map.add var v
  newenv, Null |> DataType


and evalStatement (state: VarMap) (node: Stat) =
  let errmsg = sprintf "Some strange let statement encountered: %A" node
  in
  match node with
  | Expr(e) -> state, evalExpr state e
  | Let(name, exp) -> evalLet state name exp
  | Return( v ) ->
      let retval = v
                   |> Option.defaultValue ( DataType <| Null )
                   |> ReturnValue
                   |> DataType
      state, retval
  | Block( stats ) ->
      let _, res = evalStatements state stats
      state, res |> DataType

and evalStatements state statements =
  let rec evaluator state statements =
    match statements with
    | [] -> state, Null
    | x :: [] ->
        match evalStatement state x with
        | _, DataType(ErrorMsg(msg)) -> state, ErrorMsg(msg)
        | _, DataType(ReturnValue(v)) -> state, ReturnValue(v)
        | _, DataType(v) -> state, v
    | x :: xs ->
        match evalStatement state x with
        | _, DataType(ErrorMsg(msg)) -> state, ErrorMsg(msg)
        | _, DataType(ReturnValue(v)) -> state, ReturnValue(v)
        | env, _ -> evaluator env xs
  in
  let nstate, res = evaluator state statements
  nstate, res

let eval (lst: Stat list) =
  evalStatements Map.empty lst
