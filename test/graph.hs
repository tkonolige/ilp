{-# LANGUAGE OverloadedStrings #-}
{-
From kyle's code

findVertexLabeled(StartVertex, What, StartVertex) :-
        vertex(StartVertex, What).
findVertexLabeled(StartVertex, What, ResultVertex) :-
        edge(StartVertex, NextVertex),
        not(seen(NextVertex)),
        =>(seen(NextVertex),
           findVertexLabeled(NextVertex, What, ResultVertex)).

findVertexLabeledFromStart(StartVertex, What, ResultVertex) :-
        =>(seen(StartVertex),
           findVertexLabeled(StartVertex, What, ResultVertex)).
-}
module GraphTest where

import ILP

database = createDatabase 
             [ Clause "vertex" [Atom "a", Atom "foo"] LTrue
             , Clause "vertex" [Atom "b", Atom "bar"] LTrue
             -- , Clause "vertex" [Atom "c", Atom "baz"] LTrue
             -- , Clause "vertex" [Atom "d", Atom "hello"] LTrue
             -- , Clause "edge" [Atom "a", Atom "a"] LTrue
             , Clause "edge" [Atom "a", Atom "b"] LTrue
             -- , Clause "edge" [Atom "b", Atom "c"] LTrue
             -- , Clause "edge" [Atom "c", Atom "d"] LTrue
             -- , Clause "edge" [Atom "b", Atom "a"] LTrue
             , Clause "findVertexLabeled" [Var "StartVertex", Var "What", Var "ResultVertex"]
                 (Local (Var "NextVertex")
                   (And 
                     (Check "edge" [Var "StartVertex", Var "NextVertex"])
                     (And 
                       (Not $ Check "seen" [Var "NextVertex"])
                       (Extend
                         (Clause "seen" [Var "NextVertex"] LTrue)
                         (Check "findVertexLabeled" [Var "NextVertex", Var "What", Var "ResultVertex"])
                       )
                     )
                   )
                 )
             , Clause "findVertexLabeled" [Var "StartVertex", Var "What", Var "StartVertex"]
                 (Check "vertex" [Var "StartVertex", Var "What"])
             , Clause "findVertexLabeledFromStart" [Var "StartVertex", Var "What", Var "ResultVertex"]
                 (Extend
                   (Clause "seen" [Var "StartVertex"] LTrue)
                   (Check "findVertexLabeled" [Var "StartVertex", Var "What", Var "ResultVertex"])
                 )
             ]

runTest = solve (Check "findVertexLabeledFromStart" [Atom "a", Atom "bar", Var "RES"]) database
