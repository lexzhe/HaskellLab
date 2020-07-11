{
module Synt where

import Lex
}

%name parseFirstLine File
%name parse Exp
%tokentype { Token }
%error { parseError }

%token
  var         { TVar $$ }
  '('         { TLB }
  ')'         { TRB }
  '&'         { TAnd }
  '|'         { TOr }
  "->"        { TImpl }
  '!'         { TNot }
  "|-"        { TTrn }
  ','         { TCom }
%%

File:
  Cntx "|-" Exp                 { $3 : $1 }

Cntx:
  Exp                           { [$1] }
  | Cntx ',' Exp                { $3 : $1 }
  | {- empty -}                 { [] }

Exp:
  Disj                          { $1 }
  | Disj "->" Exp               { Impl $1 $3 }

Disj:
  Conj                          { $1 }
  | Disj '|' Conj               { Or $1 $3 }

Conj:
  Term                          { $1 }
  | Conj '&' Term               { And $1 $3}

Term:
  '(' Exp ')'                   { $2 }
  | '!' Term                    { Not $2 }
  | var                         { Var $1 }

{
parseError :: [Token] -> a
parseError t = error $ "Parsing error: " ++ (show t)

data Exp = Not Exp
         | Or Exp Exp
         | And Exp Exp
         | Impl Exp Exp
         | Var String
         deriving (Eq, Ord)

instance Show Exp where
  show (And a b) = "(" ++ show a ++ " " ++ "&" ++ " " ++ show b ++ ")"
  show (Impl a b) = "(" ++ show a ++ " " ++ "->" ++ " " ++ show b ++ ")"
  show (Or a b) = "(" ++ show a ++ " " ++ "|" ++ " " ++ show b ++ ")"
  show (Not a) = "!" ++ show a
  show (Var a) = a
}