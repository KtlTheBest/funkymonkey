module Ast 

type Expr 
  = Integer of int
  | Identifier of string
  | Binary of Expr * Operator * Expr
  | Unary of Operator * Expr
  | Function of args: Expr list * body: Stat list
  | FuncCall of func: Expr * args: Expr list
  | Unit
  | Error
  | ErrorMsg of msg: string

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
  | Let of Expr
  | Return of Expr option
