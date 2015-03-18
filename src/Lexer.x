{
module Lexer where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$lower = [a-z]
$upper = [A-Z]

tokens :-

  $white+				                ;
  true                          { \s -> TTrue }
  false                         { \s -> TFalse }
  $upper [$alpha $digit \_ \']* { \s -> TVar s }
  $lower [$alpha $digit \_ \']* { \s -> TAtom s }
  \,                            { \s -> TComma }
  \;                            { \s -> TSemicolon }
  \.                            { \s -> TDot }
  \(                            { \s -> TLParens }
  \)                            { \s -> TRParens }
  =                             { \s -> TEq }
  =\>                           { \s -> TImply }
  :\-                            { \s -> TDefine }
  \\\+                          { \s -> TNot }

{
-- Each action has type :: String -> Token

-- The token type:
data Token = TVar String
           | TAtom String
           | TComma
           | TSemicolon
           | TDot
           | TLParens
           | TRParens
           | TEq
           | TImply
           | TDefine
           | TNot
           | TTrue
           | TFalse
	         deriving (Eq,Show)
}

