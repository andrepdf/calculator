-- ghci Repl.hs -package QuickCheck
-- repl

module Repl where

import Parser
import Interpreter

repl :: IO ()
repl = do
    putStr ">> "
    input <- getLine
    if input == "quit"
        then return ()
        else do
            case expression input of
                Nothing -> putStrLn "parse error"
                Just (e, "") -> print (interpret e)
                Just (e, xs) -> putStrLn ("parse error, tail: " ++ xs)
            repl
