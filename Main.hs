module Main where

import ILP
import Parser
import Pretty
import System.Console.Haskeline
import Prelude.Extras
import Data.List
import System.Environment
import qualified Data.Map as Map
import Control.Monad.Trans (lift)

parseFile file = do
  f <- readFile file
  return $ parse f

lstrip :: String -> String
lstrip (x:xs) = if elem x " \t\r\n" then strip xs else x:xs

rstrip :: String -> String
rstrip = reverse . lstrip . reverse

strip :: String -> String
strip = lstrip . rstrip

repl :: Database -> IO ()
repl database = runInputT settings (outputStrLn welcomeMessage >> loop database)
    where
      showVar (Var a) = a
      showVar (Atom a) = a
      printEnv [] = outputStrLn "Yes"
      printEnv env = mapM_ (\(a,b) -> outputStrLn $ showVar a ++ " = " ++ showVar b) env
      settings = defaultSettings {historyFile = Just ".ilp_history"}
      loop database = do
          minput <- getInputLine "?- "
          case minput of
            Nothing -> outputStrLn "Bye."
            Just input ->
              case parseCommand input of
                Print -> mapM_ (mapM_ (outputStrLn . pretty)) (Map.elems database) >> loop database
                Help -> outputStrLn helpMessage >> loop database
                Quit -> outputStrLn "Bye."
                Load filepath -> do
                  p <- lift $ parseFile $ strip filepath
                  case p of
                    Left e -> outputStrLn "Failed loading file"
                    Right db -> loop db
                Add xs ->
                  case parseClause xs of
                    Left e -> do
                      outputStrLn $ "Parse error: " ++ e
                      loop database
                    Right clause -> do
                      outputStrLn $ "Added " ++ pretty clause
                      loop (addToDatabase clause database)
                Remove xs ->
                  case parseQuery xs of
                    Left e -> do
                      outputStrLn $ "Parse error: " ++ e
                      loop database
                    Right (Check sym vars') ->
                      loop (Map.update (\xs -> filterMap $ filter (\(Clause _ vars _) -> not (vars == vars')) xs) sym database)
                        where
                          filterMap [] = Nothing
                          filterMap xs = Just xs
                Exec xs ->
                  case parseQuery xs of
                    Left e -> do
                      outputStrLn $ "Parse error: " ++ e
                      loop database
                    Right query ->
                      case solveAll query database of
                        [] -> outputStrLn "No\n." >> loop database
                        rs -> mapM_ (\xs -> printEnv xs >> outputStrLn ".") rs >> loop database

helpMessage :: String
helpMessage = "Commands\n\
              \ :+ clause\tAdds a clause to the database\n\
              \ :- clause\tRemoves a clause from the database\n\
              \ :p\t\tPrint the clause database\n\
              \ :l file\tLoads an ilp database from a file, current databse is discarded\n\
              \ :q\t\tQuits\n\
              \ :h\t\tPrint this message\n"
              ++ syntaxMessage

syntaxMessage :: String
syntaxMessage = "Syntax\n\
                \ -- this is a comment\n\
                \ fact(a). -- this is a fact\n\
                \ rule(X) :- fact(X). -- a rule\n\
                \ rule(X) Local :- fact(Local). -- declaring a local variable\n\
                \ rule(X) :- { foo(X) } => fact(X). -- implication\n\
                \ rule(X) :- \\+ fact(X). -- negation\n\
                \ rule(X) :- fact(X) , fact(a). -- conjunction\n\
                \ rule(X) :- fact(X) ; fact(a). -- disjunction\n"

welcomeMessage :: String
welcomeMessage = "Welcome to the\n\
                 \+---+ +---+ +-----------+\n\
                 \|   | |   | |           |\n\
                 \|   | |   | |           |\n\
                 \|   | |   | |           |\n\
                 \|   | |   | |    +------+\n\
                 \|   | |   | |    |       \n\
                 \|   | |   +----+ |     r \n\
                 \|   | |        | |     e \n\
                 \|   | +--------+ |     p \n\
                 \+---+       +----+     l \n\
                 \Type :h for help         \n"

data Command = Print
             | Help
             | Quit
             | Add String
             | Remove String
             | Exec String
             | Load String

parseCommand :: String -> Command
parseCommand (':':'q':_)      = Quit
parseCommand (':':'h':_)      = Help
parseCommand (':':'p':_)      = Print
parseCommand (':':'+':xs)     = Add xs
parseCommand (':':'-':xs)     = Remove xs
parseCommand (':':'l':' ':xs) = Load xs
parseCommand xs               = Exec xs

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
  then repl Map.empty
  else do
    p <- parseFile (args !! 0)
    case p of
      Left e -> putStrLn $ "Error parsing file: " ++ e
      Right database -> repl database
