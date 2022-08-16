// Implementation file for parser generated by fsyacc
module Parser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open FSharp.Text.Lexing
open FSharp.Text.Parsing.ParseHelpers
# 1 "./Parser.fsp"

open Ast

# 10 "Parser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | EOF
  | INVALID
  | TrueTok
  | FalseTok
  | NullTok
  | EQUAL
  | NOTEQ
  | LT
  | GT
  | LE
  | GE
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | COMMA
  | LET
  | FN
  | EQ
  | LSQR
  | RSQR
  | LCUR
  | RCUR
  | LPAR
  | RPAR
  | SEMICOLON
  | STR of (string)
  | INT of (string)
  | ID of (string)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_EOF
    | TOKEN_INVALID
    | TOKEN_TrueTok
    | TOKEN_FalseTok
    | TOKEN_NullTok
    | TOKEN_EQUAL
    | TOKEN_NOTEQ
    | TOKEN_LT
    | TOKEN_GT
    | TOKEN_LE
    | TOKEN_GE
    | TOKEN_ADD
    | TOKEN_SUB
    | TOKEN_MUL
    | TOKEN_DIV
    | TOKEN_MOD
    | TOKEN_COMMA
    | TOKEN_LET
    | TOKEN_FN
    | TOKEN_EQ
    | TOKEN_LSQR
    | TOKEN_RSQR
    | TOKEN_LCUR
    | TOKEN_RCUR
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_SEMICOLON
    | TOKEN_STR
    | TOKEN_INT
    | TOKEN_ID
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startStatList
    | NONTERM_StatList
    | NONTERM_Statement
    | NONTERM_Args
    | NONTERM_NonEmptyArgs
    | NONTERM_Expr
    | NONTERM_ExprFn
    | NONTERM_ExprCond
    | NONTERM_ExprComp
    | NONTERM_Expr1
    | NONTERM_Expr2
    | NONTERM_Expr3
    | NONTERM_Atom
    | NONTERM_PrimitiveTok
    | NONTERM_BoolValueTok

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | EOF  -> 0 
  | INVALID  -> 1 
  | TrueTok  -> 2 
  | FalseTok  -> 3 
  | NullTok  -> 4 
  | EQUAL  -> 5 
  | NOTEQ  -> 6 
  | LT  -> 7 
  | GT  -> 8 
  | LE  -> 9 
  | GE  -> 10 
  | ADD  -> 11 
  | SUB  -> 12 
  | MUL  -> 13 
  | DIV  -> 14 
  | MOD  -> 15 
  | COMMA  -> 16 
  | LET  -> 17 
  | FN  -> 18 
  | EQ  -> 19 
  | LSQR  -> 20 
  | RSQR  -> 21 
  | LCUR  -> 22 
  | RCUR  -> 23 
  | LPAR  -> 24 
  | RPAR  -> 25 
  | SEMICOLON  -> 26 
  | STR _ -> 27 
  | INT _ -> 28 
  | ID _ -> 29 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_EOF 
  | 1 -> TOKEN_INVALID 
  | 2 -> TOKEN_TrueTok 
  | 3 -> TOKEN_FalseTok 
  | 4 -> TOKEN_NullTok 
  | 5 -> TOKEN_EQUAL 
  | 6 -> TOKEN_NOTEQ 
  | 7 -> TOKEN_LT 
  | 8 -> TOKEN_GT 
  | 9 -> TOKEN_LE 
  | 10 -> TOKEN_GE 
  | 11 -> TOKEN_ADD 
  | 12 -> TOKEN_SUB 
  | 13 -> TOKEN_MUL 
  | 14 -> TOKEN_DIV 
  | 15 -> TOKEN_MOD 
  | 16 -> TOKEN_COMMA 
  | 17 -> TOKEN_LET 
  | 18 -> TOKEN_FN 
  | 19 -> TOKEN_EQ 
  | 20 -> TOKEN_LSQR 
  | 21 -> TOKEN_RSQR 
  | 22 -> TOKEN_LCUR 
  | 23 -> TOKEN_RCUR 
  | 24 -> TOKEN_LPAR 
  | 25 -> TOKEN_RPAR 
  | 26 -> TOKEN_SEMICOLON 
  | 27 -> TOKEN_STR 
  | 28 -> TOKEN_INT 
  | 29 -> TOKEN_ID 
  | 32 -> TOKEN_end_of_input
  | 30 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startStatList 
    | 1 -> NONTERM_StatList 
    | 2 -> NONTERM_StatList 
    | 3 -> NONTERM_StatList 
    | 4 -> NONTERM_Statement 
    | 5 -> NONTERM_Statement 
    | 6 -> NONTERM_Args 
    | 7 -> NONTERM_Args 
    | 8 -> NONTERM_NonEmptyArgs 
    | 9 -> NONTERM_NonEmptyArgs 
    | 10 -> NONTERM_Expr 
    | 11 -> NONTERM_Expr 
    | 12 -> NONTERM_ExprFn 
    | 13 -> NONTERM_ExprFn 
    | 14 -> NONTERM_ExprCond 
    | 15 -> NONTERM_ExprCond 
    | 16 -> NONTERM_ExprCond 
    | 17 -> NONTERM_ExprComp 
    | 18 -> NONTERM_ExprComp 
    | 19 -> NONTERM_ExprComp 
    | 20 -> NONTERM_ExprComp 
    | 21 -> NONTERM_ExprComp 
    | 22 -> NONTERM_Expr1 
    | 23 -> NONTERM_Expr1 
    | 24 -> NONTERM_Expr1 
    | 25 -> NONTERM_Expr2 
    | 26 -> NONTERM_Expr2 
    | 27 -> NONTERM_Expr2 
    | 28 -> NONTERM_Expr2 
    | 29 -> NONTERM_Expr3 
    | 30 -> NONTERM_Atom 
    | 31 -> NONTERM_Atom 
    | 32 -> NONTERM_Atom 
    | 33 -> NONTERM_Atom 
    | 34 -> NONTERM_Atom 
    | 35 -> NONTERM_Atom 
    | 36 -> NONTERM_Atom 
    | 37 -> NONTERM_Atom 
    | 38 -> NONTERM_PrimitiveTok 
    | 39 -> NONTERM_PrimitiveTok 
    | 40 -> NONTERM_BoolValueTok 
    | 41 -> NONTERM_BoolValueTok 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 32 
let _fsyacc_tagOfErrorTerminal = 30

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | EOF  -> "EOF" 
  | INVALID  -> "INVALID" 
  | TrueTok  -> "TrueTok" 
  | FalseTok  -> "FalseTok" 
  | NullTok  -> "NullTok" 
  | EQUAL  -> "EQUAL" 
  | NOTEQ  -> "NOTEQ" 
  | LT  -> "LT" 
  | GT  -> "GT" 
  | LE  -> "LE" 
  | GE  -> "GE" 
  | ADD  -> "ADD" 
  | SUB  -> "SUB" 
  | MUL  -> "MUL" 
  | DIV  -> "DIV" 
  | MOD  -> "MOD" 
  | COMMA  -> "COMMA" 
  | LET  -> "LET" 
  | FN  -> "FN" 
  | EQ  -> "EQ" 
  | LSQR  -> "LSQR" 
  | RSQR  -> "RSQR" 
  | LCUR  -> "LCUR" 
  | RCUR  -> "RCUR" 
  | LPAR  -> "LPAR" 
  | RPAR  -> "RPAR" 
  | SEMICOLON  -> "SEMICOLON" 
  | STR _ -> "STR" 
  | INT _ -> "INT" 
  | ID _ -> "ID" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | EOF  -> (null : System.Object) 
  | INVALID  -> (null : System.Object) 
  | TrueTok  -> (null : System.Object) 
  | FalseTok  -> (null : System.Object) 
  | NullTok  -> (null : System.Object) 
  | EQUAL  -> (null : System.Object) 
  | NOTEQ  -> (null : System.Object) 
  | LT  -> (null : System.Object) 
  | GT  -> (null : System.Object) 
  | LE  -> (null : System.Object) 
  | GE  -> (null : System.Object) 
  | ADD  -> (null : System.Object) 
  | SUB  -> (null : System.Object) 
  | MUL  -> (null : System.Object) 
  | DIV  -> (null : System.Object) 
  | MOD  -> (null : System.Object) 
  | COMMA  -> (null : System.Object) 
  | LET  -> (null : System.Object) 
  | FN  -> (null : System.Object) 
  | EQ  -> (null : System.Object) 
  | LSQR  -> (null : System.Object) 
  | RSQR  -> (null : System.Object) 
  | LCUR  -> (null : System.Object) 
  | RCUR  -> (null : System.Object) 
  | LPAR  -> (null : System.Object) 
  | RPAR  -> (null : System.Object) 
  | SEMICOLON  -> (null : System.Object) 
  | STR _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | INT _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | ID _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 3us; 65535us; 0us; 1us; 3us; 4us; 21us; 22us; 3us; 65535us; 0us; 3us; 3us; 3us; 21us; 3us; 3us; 65535us; 18us; 19us; 56us; 57us; 63us; 64us; 4us; 65535us; 12us; 13us; 18us; 10us; 56us; 10us; 63us; 10us; 4us; 65535us; 0us; 5us; 3us; 5us; 7us; 8us; 21us; 5us; 21us; 65535us; 0us; 16us; 3us; 16us; 7us; 16us; 12us; 11us; 14us; 15us; 18us; 11us; 21us; 16us; 28us; 60us; 29us; 60us; 35us; 60us; 36us; 60us; 37us; 60us; 38us; 60us; 39us; 60us; 41us; 60us; 44us; 60us; 46us; 60us; 48us; 60us; 56us; 11us; 61us; 59us; 63us; 11us; 21us; 65535us; 0us; 24us; 3us; 24us; 7us; 24us; 12us; 24us; 14us; 24us; 18us; 24us; 21us; 24us; 28us; 24us; 29us; 24us; 35us; 24us; 36us; 24us; 37us; 24us; 38us; 24us; 39us; 24us; 41us; 24us; 44us; 24us; 46us; 24us; 48us; 24us; 56us; 24us; 61us; 24us; 63us; 24us; 21us; 65535us; 0us; 27us; 3us; 27us; 7us; 27us; 12us; 27us; 14us; 27us; 18us; 27us; 21us; 27us; 28us; 25us; 29us; 26us; 35us; 27us; 36us; 27us; 37us; 27us; 38us; 27us; 39us; 27us; 41us; 27us; 44us; 27us; 46us; 27us; 48us; 27us; 56us; 27us; 61us; 27us; 63us; 27us; 21us; 65535us; 0us; 34us; 3us; 34us; 7us; 34us; 12us; 34us; 14us; 34us; 18us; 34us; 21us; 34us; 28us; 34us; 29us; 34us; 35us; 30us; 36us; 31us; 37us; 32us; 38us; 33us; 39us; 34us; 41us; 34us; 44us; 34us; 46us; 34us; 48us; 34us; 56us; 34us; 61us; 34us; 63us; 34us; 21us; 65535us; 0us; 43us; 3us; 43us; 7us; 43us; 12us; 43us; 14us; 43us; 18us; 43us; 21us; 43us; 28us; 43us; 29us; 43us; 35us; 43us; 36us; 43us; 37us; 43us; 38us; 43us; 39us; 40us; 41us; 42us; 44us; 43us; 46us; 43us; 48us; 43us; 56us; 43us; 61us; 43us; 63us; 43us; 21us; 65535us; 0us; 50us; 3us; 50us; 7us; 50us; 12us; 50us; 14us; 50us; 18us; 50us; 21us; 50us; 28us; 50us; 29us; 50us; 35us; 50us; 36us; 50us; 37us; 50us; 38us; 50us; 39us; 50us; 41us; 50us; 44us; 45us; 46us; 47us; 48us; 49us; 56us; 50us; 61us; 50us; 63us; 50us; 21us; 65535us; 0us; 51us; 3us; 51us; 7us; 51us; 12us; 51us; 14us; 51us; 18us; 51us; 21us; 51us; 28us; 51us; 29us; 51us; 35us; 51us; 36us; 51us; 37us; 51us; 38us; 51us; 39us; 51us; 41us; 51us; 44us; 51us; 46us; 51us; 48us; 51us; 56us; 51us; 61us; 51us; 63us; 51us; 21us; 65535us; 0us; 55us; 3us; 55us; 7us; 55us; 12us; 55us; 14us; 55us; 18us; 55us; 21us; 55us; 28us; 55us; 29us; 55us; 35us; 55us; 36us; 55us; 37us; 55us; 38us; 55us; 39us; 55us; 41us; 55us; 44us; 55us; 46us; 55us; 48us; 55us; 56us; 55us; 61us; 55us; 63us; 55us; 21us; 65535us; 0us; 68us; 3us; 68us; 7us; 68us; 12us; 68us; 14us; 68us; 18us; 68us; 21us; 68us; 28us; 68us; 29us; 68us; 35us; 68us; 36us; 68us; 37us; 68us; 38us; 68us; 39us; 68us; 41us; 68us; 44us; 68us; 46us; 68us; 48us; 68us; 56us; 68us; 61us; 68us; 63us; 68us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 5us; 9us; 13us; 18us; 23us; 45us; 67us; 89us; 111us; 133us; 155us; 177us; 199us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 1us; 2us; 1us; 3us; 1us; 3us; 2us; 4us; 10us; 1us; 4us; 1us; 5us; 2us; 5us; 10us; 1us; 5us; 1us; 7us; 4us; 8us; 9us; 35us; 36us; 1us; 9us; 1us; 9us; 1us; 10us; 3us; 10us; 35us; 36us; 3us; 11us; 35us; 36us; 1us; 12us; 1us; 12us; 1us; 12us; 1us; 12us; 1us; 12us; 1us; 12us; 1us; 12us; 1us; 13us; 4us; 14us; 14us; 15us; 16us; 4us; 14us; 15us; 15us; 16us; 3us; 14us; 15us; 16us; 1us; 14us; 1us; 15us; 8us; 17us; 17us; 18us; 19us; 20us; 21us; 22us; 23us; 8us; 17us; 18us; 18us; 19us; 20us; 21us; 22us; 23us; 8us; 17us; 18us; 19us; 19us; 20us; 21us; 22us; 23us; 8us; 17us; 18us; 19us; 20us; 20us; 21us; 22us; 23us; 7us; 17us; 18us; 19us; 20us; 21us; 22us; 23us; 1us; 17us; 1us; 18us; 1us; 19us; 1us; 20us; 1us; 22us; 5us; 22us; 24us; 25us; 26us; 27us; 1us; 23us; 5us; 23us; 24us; 25us; 26us; 27us; 4us; 24us; 25us; 26us; 27us; 1us; 25us; 2us; 25us; 28us; 1us; 26us; 2us; 26us; 28us; 1us; 27us; 2us; 27us; 28us; 1us; 28us; 1us; 29us; 1us; 30us; 1us; 31us; 1us; 32us; 1us; 33us; 1us; 34us; 1us; 34us; 1us; 34us; 3us; 35us; 35us; 36us; 2us; 35us; 36us; 1us; 35us; 1us; 35us; 1us; 36us; 1us; 36us; 1us; 36us; 1us; 37us; 1us; 38us; 1us; 39us; 1us; 40us; 1us; 41us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 6us; 8us; 10us; 13us; 15us; 17us; 20us; 22us; 24us; 29us; 31us; 33us; 35us; 39us; 43us; 45us; 47us; 49us; 51us; 53us; 55us; 57us; 59us; 64us; 69us; 73us; 75us; 77us; 86us; 95us; 104us; 113us; 121us; 123us; 125us; 127us; 129us; 131us; 137us; 139us; 145us; 150us; 152us; 155us; 157us; 160us; 162us; 165us; 167us; 169us; 171us; 173us; 175us; 177us; 179us; 181us; 183us; 187us; 190us; 192us; 194us; 196us; 198us; 200us; 202us; 204us; 206us; 208us; |]
let _fsyacc_action_rows = 71
let _fsyacc_actionTableElements = [|11us; 16385us; 0us; 2us; 1us; 66us; 2us; 69us; 3us; 70us; 4us; 67us; 17us; 7us; 18us; 17us; 20us; 56us; 27us; 54us; 28us; 52us; 29us; 53us; 0us; 49152us; 0us; 16386us; 11us; 16385us; 0us; 2us; 1us; 66us; 2us; 69us; 3us; 70us; 4us; 67us; 17us; 7us; 18us; 17us; 20us; 56us; 27us; 54us; 28us; 52us; 29us; 53us; 0us; 16387us; 2us; 32768us; 19us; 14us; 26us; 6us; 0us; 16388us; 9us; 32768us; 1us; 66us; 2us; 69us; 3us; 70us; 4us; 67us; 18us; 17us; 20us; 56us; 27us; 54us; 28us; 52us; 29us; 53us; 2us; 32768us; 19us; 14us; 26us; 9us; 0us; 16389us; 0us; 16391us; 3us; 16392us; 16us; 12us; 20us; 61us; 24us; 63us; 9us; 32768us; 1us; 66us; 2us; 69us; 3us; 70us; 4us; 67us; 18us; 17us; 20us; 56us; 27us; 54us; 28us; 52us; 29us; 53us; 0us; 16393us; 9us; 32768us; 1us; 66us; 2us; 69us; 3us; 70us; 4us; 67us; 18us; 17us; 20us; 56us; 27us; 54us; 28us; 52us; 29us; 53us; 2us; 16394us; 20us; 61us; 24us; 63us; 2us; 16395us; 20us; 61us; 24us; 63us; 1us; 32768us; 24us; 18us; 9us; 16390us; 1us; 66us; 2us; 69us; 3us; 70us; 4us; 67us; 18us; 17us; 20us; 56us; 27us; 54us; 28us; 52us; 29us; 53us; 1us; 32768us; 25us; 20us; 1us; 32768us; 22us; 21us; 11us; 16385us; 0us; 2us; 1us; 66us; 2us; 69us; 3us; 70us; 4us; 67us; 17us; 7us; 18us; 17us; 20us; 56us; 27us; 54us; 28us; 52us; 29us; 53us; 1us; 32768us; 23us; 23us; 0us; 16396us; 0us; 16397us; 2us; 16398us; 5us; 28us; 6us; 29us; 2us; 16399us; 5us; 28us; 6us; 29us; 2us; 16400us; 5us; 28us; 6us; 29us; 9us; 32768us; 1us; 66us; 2us; 69us; 3us; 70us; 4us; 67us; 18us; 17us; 20us; 56us; 27us; 54us; 28us; 52us; 29us; 53us; 9us; 32768us; 1us; 66us; 2us; 69us; 3us; 70us; 4us; 67us; 18us; 17us; 20us; 56us; 27us; 54us; 28us; 52us; 29us; 53us; 6us; 16401us; 7us; 35us; 8us; 36us; 9us; 37us; 10us; 38us; 11us; 39us; 12us; 41us; 6us; 16402us; 7us; 35us; 8us; 36us; 9us; 37us; 10us; 38us; 11us; 39us; 12us; 41us; 6us; 16403us; 7us; 35us; 8us; 36us; 9us; 37us; 10us; 38us; 11us; 39us; 12us; 41us; 6us; 16404us; 7us; 35us; 8us; 36us; 9us; 37us; 10us; 38us; 11us; 39us; 12us; 41us; 6us; 16405us; 7us; 35us; 8us; 36us; 9us; 37us; 10us; 38us; 11us; 39us; 12us; 41us; 9us; 32768us; 1us; 66us; 2us; 69us; 3us; 70us; 4us; 67us; 18us; 17us; 20us; 56us; 27us; 54us; 28us; 52us; 29us; 53us; 9us; 32768us; 1us; 66us; 2us; 69us; 3us; 70us; 4us; 67us; 18us; 17us; 20us; 56us; 27us; 54us; 28us; 52us; 29us; 53us; 9us; 32768us; 1us; 66us; 2us; 69us; 3us; 70us; 4us; 67us; 18us; 17us; 20us; 56us; 27us; 54us; 28us; 52us; 29us; 53us; 9us; 32768us; 1us; 66us; 2us; 69us; 3us; 70us; 4us; 67us; 18us; 17us; 20us; 56us; 27us; 54us; 28us; 52us; 29us; 53us; 9us; 32768us; 1us; 66us; 2us; 69us; 3us; 70us; 4us; 67us; 18us; 17us; 20us; 56us; 27us; 54us; 28us; 52us; 29us; 53us; 3us; 16406us; 13us; 44us; 14us; 46us; 15us; 48us; 9us; 32768us; 1us; 66us; 2us; 69us; 3us; 70us; 4us; 67us; 18us; 17us; 20us; 56us; 27us; 54us; 28us; 52us; 29us; 53us; 3us; 16407us; 13us; 44us; 14us; 46us; 15us; 48us; 3us; 16408us; 13us; 44us; 14us; 46us; 15us; 48us; 9us; 32768us; 1us; 66us; 2us; 69us; 3us; 70us; 4us; 67us; 18us; 17us; 20us; 56us; 27us; 54us; 28us; 52us; 29us; 53us; 0us; 16409us; 9us; 32768us; 1us; 66us; 2us; 69us; 3us; 70us; 4us; 67us; 18us; 17us; 20us; 56us; 27us; 54us; 28us; 52us; 29us; 53us; 0us; 16410us; 9us; 32768us; 1us; 66us; 2us; 69us; 3us; 70us; 4us; 67us; 18us; 17us; 20us; 56us; 27us; 54us; 28us; 52us; 29us; 53us; 0us; 16411us; 0us; 16412us; 0us; 16413us; 0us; 16414us; 0us; 16415us; 0us; 16416us; 0us; 16417us; 9us; 16390us; 1us; 66us; 2us; 69us; 3us; 70us; 4us; 67us; 18us; 17us; 20us; 56us; 27us; 54us; 28us; 52us; 29us; 53us; 1us; 32768us; 21us; 58us; 0us; 16418us; 3us; 32768us; 20us; 61us; 21us; 62us; 24us; 63us; 2us; 32768us; 20us; 61us; 24us; 63us; 9us; 32768us; 1us; 66us; 2us; 69us; 3us; 70us; 4us; 67us; 18us; 17us; 20us; 56us; 27us; 54us; 28us; 52us; 29us; 53us; 0us; 16419us; 9us; 16390us; 1us; 66us; 2us; 69us; 3us; 70us; 4us; 67us; 18us; 17us; 20us; 56us; 27us; 54us; 28us; 52us; 29us; 53us; 1us; 32768us; 25us; 65us; 0us; 16420us; 0us; 16421us; 0us; 16422us; 0us; 16423us; 0us; 16424us; 0us; 16425us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 12us; 13us; 14us; 26us; 27us; 30us; 31us; 41us; 44us; 45us; 46us; 50us; 60us; 61us; 71us; 74us; 77us; 79us; 89us; 91us; 93us; 105us; 107us; 108us; 109us; 112us; 115us; 118us; 128us; 138us; 145us; 152us; 159us; 166us; 173us; 183us; 193us; 203us; 213us; 223us; 227us; 237us; 241us; 245us; 255us; 256us; 266us; 267us; 277us; 278us; 279us; 280us; 281us; 282us; 283us; 284us; 294us; 296us; 297us; 301us; 304us; 314us; 315us; 325us; 327us; 328us; 329us; 330us; 331us; 332us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 0us; 1us; 2us; 2us; 3us; 0us; 1us; 1us; 3us; 3us; 1us; 7us; 1us; 3us; 3us; 1us; 3us; 3us; 3us; 3us; 1us; 3us; 3us; 1us; 3us; 3us; 3us; 1us; 1us; 1us; 1us; 1us; 1us; 3us; 4us; 4us; 1us; 1us; 1us; 1us; 1us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 1us; 1us; 2us; 2us; 3us; 3us; 4us; 4us; 5us; 5us; 6us; 6us; 7us; 7us; 7us; 8us; 8us; 8us; 8us; 8us; 9us; 9us; 9us; 10us; 10us; 10us; 10us; 11us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 13us; 13us; 14us; 14us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 16386us; 65535us; 16387us; 65535us; 16388us; 65535us; 65535us; 16389us; 16391us; 65535us; 65535us; 16393us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16396us; 16397us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16412us; 16413us; 16414us; 16415us; 16416us; 16417us; 65535us; 65535us; 16418us; 65535us; 65535us; 65535us; 16419us; 65535us; 65535us; 16420us; 16421us; 16422us; 16423us; 16424us; 16425us; |]
let _fsyacc_reductions ()  =    [| 
# 294 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Stat list in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : 'gentype__startStatList));
# 303 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 41 "./Parser.fsp"
                             [] 
                   )
# 41 "./Parser.fsp"
                 : Stat list));
# 313 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 42 "./Parser.fsp"
                             [] 
                   )
# 42 "./Parser.fsp"
                 : Stat list));
# 323 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Stat in
            let _2 = parseState.GetInput(2) :?> Stat list in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 43 "./Parser.fsp"
                                            _1 :: _2 
                   )
# 43 "./Parser.fsp"
                 : Stat list));
# 335 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 46 "./Parser.fsp"
                                            Expr( _1 ) 
                   )
# 46 "./Parser.fsp"
                 : Stat));
# 346 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> Expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 47 "./Parser.fsp"
                                            Let( _2 ) 
                   )
# 47 "./Parser.fsp"
                 : Stat));
# 357 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 50 "./Parser.fsp"
                                      [] 
                   )
# 50 "./Parser.fsp"
                 : Expr list));
# 367 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Expr list in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 51 "./Parser.fsp"
                                      _1 
                   )
# 51 "./Parser.fsp"
                 : Expr list));
# 378 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 54 "./Parser.fsp"
                                                   [ _1 ] 
                   )
# 54 "./Parser.fsp"
                 : Expr list));
# 389 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Expr in
            let _3 = parseState.GetInput(3) :?> Expr list in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 55 "./Parser.fsp"
                                                   _1 :: _3 
                   )
# 55 "./Parser.fsp"
                 : Expr list));
# 401 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Expr in
            let _3 = parseState.GetInput(3) :?> Expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 58 "./Parser.fsp"
                                        Binary( _1, Assign, _3 ) 
                   )
# 58 "./Parser.fsp"
                 : Expr));
# 413 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 59 "./Parser.fsp"
                                        _1 
                   )
# 59 "./Parser.fsp"
                 : Expr));
# 424 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _3 = parseState.GetInput(3) :?> Expr list in
            let _6 = parseState.GetInput(6) :?> Stat list in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 62 "./Parser.fsp"
                                                              FuncDef( _3, _6 ) 
                   )
# 62 "./Parser.fsp"
                 : Expr));
# 436 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_ExprCond in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 63 "./Parser.fsp"
                                  _1 
                   )
# 63 "./Parser.fsp"
                 : Expr));
# 447 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_ExprComp in
            let _3 = parseState.GetInput(3) :?> 'gentype_ExprComp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 66 "./Parser.fsp"
                                                 Binary( _1, Equal,    _3 ) 
                   )
# 66 "./Parser.fsp"
                 : 'gentype_ExprCond));
# 459 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_ExprComp in
            let _3 = parseState.GetInput(3) :?> 'gentype_ExprComp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 67 "./Parser.fsp"
                                                 Binary( _1, NotEqual, _3 ) 
                   )
# 67 "./Parser.fsp"
                 : 'gentype_ExprCond));
# 471 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_ExprComp in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 68 "./Parser.fsp"
                                                 _1 
                   )
# 68 "./Parser.fsp"
                 : 'gentype_ExprCond));
# 482 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Expr in
            let _3 = parseState.GetInput(3) :?> Expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 71 "./Parser.fsp"
                                        Binary( _1, LessThan,       _3 ) 
                   )
# 71 "./Parser.fsp"
                 : 'gentype_ExprComp));
# 494 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Expr in
            let _3 = parseState.GetInput(3) :?> Expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 72 "./Parser.fsp"
                                        Binary( _1, LessOrEqual,    _3 ) 
                   )
# 72 "./Parser.fsp"
                 : 'gentype_ExprComp));
# 506 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Expr in
            let _3 = parseState.GetInput(3) :?> Expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 73 "./Parser.fsp"
                                        Binary( _1, GreaterThan,    _3 ) 
                   )
# 73 "./Parser.fsp"
                 : 'gentype_ExprComp));
# 518 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Expr in
            let _3 = parseState.GetInput(3) :?> Expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 74 "./Parser.fsp"
                                        Binary( _1, GreaterOrEqual, _3 ) 
                   )
# 74 "./Parser.fsp"
                 : 'gentype_ExprComp));
# 530 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 75 "./Parser.fsp"
                               _1 
                   )
# 75 "./Parser.fsp"
                 : 'gentype_ExprComp));
# 541 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Expr in
            let _3 = parseState.GetInput(3) :?> Expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 78 "./Parser.fsp"
                                         Binary( _1, Add, _3 ) 
                   )
# 78 "./Parser.fsp"
                 : Expr));
# 553 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Expr in
            let _3 = parseState.GetInput(3) :?> Expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 79 "./Parser.fsp"
                                         Binary( _1, Sub, _3 ) 
                   )
# 79 "./Parser.fsp"
                 : Expr));
# 565 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 80 "./Parser.fsp"
                                         _1 
                   )
# 80 "./Parser.fsp"
                 : Expr));
# 576 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Expr in
            let _3 = parseState.GetInput(3) :?> Expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 83 "./Parser.fsp"
                                         Binary( _1, Mul, _3 ) 
                   )
# 83 "./Parser.fsp"
                 : Expr));
# 588 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Expr in
            let _3 = parseState.GetInput(3) :?> Expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 84 "./Parser.fsp"
                                         Binary( _1, Div, _3 ) 
                   )
# 84 "./Parser.fsp"
                 : Expr));
# 600 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Expr in
            let _3 = parseState.GetInput(3) :?> Expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 85 "./Parser.fsp"
                                         Binary( _1, Mod, _3 ) 
                   )
# 85 "./Parser.fsp"
                 : Expr));
# 612 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 86 "./Parser.fsp"
                                         _1 
                   )
# 86 "./Parser.fsp"
                 : Expr));
# 623 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 88 "./Parser.fsp"
                                   _1 
                   )
# 88 "./Parser.fsp"
                 : Expr));
# 634 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 91 "./Parser.fsp"
                                                 Integer( _1 |> int ) 
                   )
# 91 "./Parser.fsp"
                 : Expr));
# 645 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 92 "./Parser.fsp"
                                                 Identifier( _1 ) 
                   )
# 92 "./Parser.fsp"
                 : Expr));
# 656 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 93 "./Parser.fsp"
                                                 String( _1 ) 
                   )
# 93 "./Parser.fsp"
                 : Expr));
# 667 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Primitive in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 94 "./Parser.fsp"
                                                 _1 |> Primitive 
                   )
# 94 "./Parser.fsp"
                 : Expr));
# 678 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> Expr list in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 95 "./Parser.fsp"
                                                 Array( _2 ) 
                   )
# 95 "./Parser.fsp"
                 : Expr));
# 689 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Expr in
            let _3 = parseState.GetInput(3) :?> Expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 96 "./Parser.fsp"
                                                 IndexAccess( _3, _1 ) 
                   )
# 96 "./Parser.fsp"
                 : Expr));
# 701 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Expr in
            let _3 = parseState.GetInput(3) :?> Expr list in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 97 "./Parser.fsp"
                                                 FuncCall( _1, _3 ) 
                   )
# 97 "./Parser.fsp"
                 : Expr));
# 713 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 98 "./Parser.fsp"
                                                 Error 
                   )
# 98 "./Parser.fsp"
                 : Expr));
# 723 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 101 "./Parser.fsp"
                                  Null 
                   )
# 101 "./Parser.fsp"
                 : Primitive));
# 733 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> BoolValue in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 102 "./Parser.fsp"
                                      _1 |> BoolValue 
                   )
# 102 "./Parser.fsp"
                 : Primitive));
# 744 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 105 "./Parser.fsp"
                                  True 
                   )
# 105 "./Parser.fsp"
                 : BoolValue));
# 754 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 106 "./Parser.fsp"
                                  False 
                   )
# 106 "./Parser.fsp"
                 : BoolValue));
|]
# 765 "Parser.fs"
let tables : FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 33;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = tables.Interpret(lexer, lexbuf, startState)
let StatList lexer lexbuf : Stat list =
    engine lexer lexbuf 0 :?> _