{
module Parser (parse, parseQuery) where

import ILP
import Lexer
}

%name parseClauses Clauses
%name parseStatement Statement
%tokentype { Token }
%monad { Either String }
%error { parseError }
%left ';'
%left ','
%right '=>'
%left '\+'

%token
      true            { TTrue }
      false           { TFalse }
      var             { TVar $$ }
      atom            { TAtom $$ }
      ','             { TComma }
      ';'             { TSemicolon }
      '.'             { TDot }
      ':-'            { TDefine }
      '('             { TLParens }
      ')'             { TRParens }
      '='             { TEq }
      '=>'            { TImply }
      '\+'            { TNot }
%%

Clauses : Clause Clauses                          { $1 : $2 }
        | {- empty-}                              { [] }

Clause : Fact '.'                                 { $1 }
       | Rule '.'                                 { $1 }

Fact : atom '(' ArgsAtom ')'                      { Clause $1 $3 LTrue }

ArgsAtom : atom                                   { [Atom $1] }
         | atom ',' ArgsAtom                      { (Atom $1) : $3 }

ArgsVar : var                                     { [Var $1] }
        | var ',' ArgsVar                         { (Var $1) : $3 }

ArgsMixed : var                                   { [Var $1] }
          | atom                                  { [Atom $1] }
          | var ',' ArgsMixed                     { (Var $1) : $3 }
          | atom ',' ArgsMixed                    { (Atom $1) : $3 }

Locals : var Locals                               { (Var $1) : $2 }
       | {- empty -}                              { [] }

Rule : atom '(' ArgsVar ')' Locals ':-' Statement { Clause $1 $3 (foldr (Local) $7 $5) }

Var : var                                         { Var $1}
    | atom                                        { Atom $1 }

Statement : Statement ',' Statement               { And $1 $3 }
          | Statement ';' Statement               { Or $1 $3 }
          | '(' Statement ')'                     { $2 }
          | Fact '=>' Statement                   { Extend $1 $3 }
          | Var '=' Var                           { Unify $1 $3 }
          | '\+' Statement                        { Not $2 }
          | atom '(' ArgsMixed ')'                { Check $1 $3 }
          | true                                  { LTrue }
          | false                                 { LFalse }

{
parseError tkns = Left $ show tkns

parse = fmap createDatabase . parseClauses . alexScanTokens
parseQuery = parseStatement . alexScanTokens
}

