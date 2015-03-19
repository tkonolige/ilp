{
module Parser (parse, parseQueryOrClause) where

import ILP
import Lexer
}

%name parseClauses Clauses
%name parseClauseOrStatement ClauseOrStatement
%tokentype { Token }
%monad { Either String }
%error { parseError }
%left ';'
%left ','
%right '=>'
%left '\+'

%token
      true            { TTrue      }
      false           { TFalse     }
      var             { TVar $$    }
      atom            { TAtom $$   }
      ','             { TComma     }
      ';'             { TSemicolon }
      '.'             { TDot       }
      ':-'            { TDefine    }
      '('             { TLParens   }
      ')'             { TRParens   }
      '='             { TEq        }
      '=>'            { TImply     }
      '\+'            { TNot       }
      '{'             { TLCurly    }
      '}'             { TRCurly    }
      '+'             { TPlus      }
%%

Clauses : Clause Clauses                       { $1 : $2 }
        | {- empty-}                           { [] }

Clause : Fact '.'                              { $1 }
       | Rule '.'                              { $1 }

Fact : atom '(' Args ')'                       { Clause $1 $3 LTrue }

Args: var                                      { [Var $1] }
          | atom                               { [Atom $1] }
          | var ',' Args                       { (Var $1) : $3 }
          | atom ',' Args                      { (Atom $1) : $3 }

Locals : var Locals                            { (Var $1) : $2 }
       | {- empty -}                           { [] }

Rule : atom '(' Args ')' Locals ':-' Statement { Clause $1 $3 (foldr (Local) $7 $5) }

Var : var                                      { Var $1}
    | atom                                     { Atom $1 }

Imply : Clause                                 { $1 }
      | atom '(' Args ')'                      { Clause $1 $3 LTrue }

Statement : Statement ',' Statement            { And $1 $3 }
          | Statement ';' Statement            { Or $1 $3 }
          | '(' Statement ')'                  { $2 }
          | '{' Imply '}' '=>' Statement       { Extend $2 $5 }
          | Var '=' Var                        { Unify $1 $3 }
          | '\+' Statement                     { Not $2 }
          | atom '(' Args ')'                  { Check $1 $3 }
          | true                               { LTrue }
          | false                              { LFalse }

ClauseOrStatement : '+' Clause                 { Left $2 }
                  | Statement                  { Right $1 }

{
parseError tkns = Left $ show tkns

parse = fmap createDatabase . parseClauses . alexScanTokens
parseQueryOrClause = parseClauseOrStatement . alexScanTokens
}

