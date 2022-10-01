module Ast

type DataType
  = Null
  | Identifier of string
  | Boolean of bool
  | Integer of int
  | String of string
  | Array of (Expr list)
  | Function of args: Expr list * block: Stat list * env: Map<string, Expr>
  | ReturnValue of Expr
  | ErrorMsg of string
  | Error

and Application
  = FuncCall of f: Expr * args: Expr list
  | FuncDef of args: Expr list * block: Stat list
  | IndexAccess of index: Expr * array: Expr
  | Binary of a: Expr * op: Operator * b: Expr
  | Unary of op: Operator * e: Expr
  | If of cond: Expr * truebranch: Stat list * falsebranch: Stat list

and Expr =
  | DataType of DataType
  | Application of Application

and Operator
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | Assign
  | Equal
  | NotEqual
  | LessThan
  | GreaterThan
  | LessOrEqual
  | GreaterOrEqual
  | BitAnd
  | BitOr
  | BitXor
  | LogicalAnd
  | LogicalOr

and Stat
  = Expr of Expr
  | Let of name: string * value: Expr
  | Block of Stat list
  | Return of Expr option
