{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import ILP
import ClassyPrelude hiding (union, pack, unpack)
import Text.Parsec hiding ((<|>))
import Text.Parsec.Text
import Data.Text (pack, unpack)

import Data.List ((!!))

var :: Parser Variable
var = do
  u <- upper 
  rest <- many alphaNum
  return $ Var $! pack $! u:rest

atom :: Parser Variable
atom = do
  l <- lower
  rest <- many alphaNum
  return $ Atom $! pack $! l:rest

variable = var <|> atom

variables = many1 $ do
                     v <- variable
                     spaces
                     return v

symbol :: Parser Symbol
symbol = do
  l <- letter
  rest <- many alphaNum
  return $ pack $ l:rest 

clause = do
  s <- symbol
  spaces
  args <- variables
  string ":-"
  spaces
  b <- body
  spaces
  char '.'
  return $ Clause s args b

andC = do
  b1 <- body
  spaces
  char ','
  spaces
  b2 <- body
  return $ And b1 b2

orC = do
  b1 <- body
  spaces
  char ';'
  spaces
  b2 <-body
  return $ Or b1 b2

check = do
  s <- symbol
  spaces
  args <- variables
  return $ Check s args

unifyC = do
  v1 <- variable
  spaces
  char '='
  spaces
  v2 <- variable
  return $ Unify v1 v2

true = do
  string "true"
  return LTrue

false = do
  string "false"
  return LFalse

notC = do
  string "\\+"
  spaces
  b <- body
  return $ Not b

extend = do
  c <- clause
  spaces
  string "=>"
  spaces
  b <- body
  return $ Extend c b

body = true <|> false <|> andC <|> check <|> extend <|> unifyC <|> notC <|> orC

query = do
  s <- symbol
  spaces
  args <- variables
  char '?'
  return $ Check s args

program = do
  clauses <- many1 $ do
                      c <- clause
                      spaces
                      return c
  q <- query
  return (createDatabase clauses, q)

parseAndRun file = do
  f <- readFile (fpFromText file) 
  return $ case parse program (unpack file) f of
             Left err ->  Left err
             Right (database, query) -> Right $ solveAll query database

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1 then putStrLn "usage: ilp program" else parseAndRun (args !! 0) >>= print
