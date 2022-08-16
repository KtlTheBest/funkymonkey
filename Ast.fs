module Ast 

type BoolValue
  = True
  | False

type Primitive = Null | BoolValue of BoolValue

type Expr 
  = Primitive of Primitive
  | Unit
  | Integer of int
  | String of string
  | Identifier of string
  | Array of Expr list
  | Function of args: Expr list * body: Stat list * env: Map<string, Expr>
  | FuncDef of args: Expr list * body: Stat list
  | FuncCall of func: Expr * args: Expr list
  | Error
  | ErrorMsg of msg: string
  | Binary of a: Expr * op: Operator * b: Expr
  | Unary of op: Operator * e: Expr
  | IndexAccess of indx: Expr * arr: Expr

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
  | Block of Stat list
  | If of cond: Expr * truebranch: Stat list * falsebranch: Stat list
  | Return of Expr option
