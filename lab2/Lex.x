{
module Lex where
}

%wrapper "basic"

$digit = 0-9
$alpha = A-Z

tokens :-

  $white+                                     ;
  \(                                          { \s -> TLB }
  \)                                          { \s -> TRB }
  &                                           { \s -> TAnd }
  \|                                          { \s -> TOr }
  "->"                                        { \s -> TImpl }
  !                                           { \s -> TNot }
  \,                                          {\s -> TCom}
  "|-"                                        {\s -> TTrn}
  $alpha [$alpha $digit \_ \']*               { \s -> TVar s}

{
data Token = TLB
           | TRB
           | TTrn
           | TCom
           | TOr
           | TAnd
           | TVar String
           | TNot
           | TImpl
           deriving (Eq, Show)
}