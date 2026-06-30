module Repl
    ( repl )
where

import Interpreter ( interpret )
import Parser      ( Parser(..), expression )

repl :: IO ()
repl = do
    putStr ">> "
    input <- getLine
    if null input
        then repl
        else if input `elem` [":q", ":quit", "q", "quit"]
            then return ()
            else do
                case parse expression input of
                    Nothing     -> putStrLn "Parse Error"
                    Just (e, _) ->
                        case interpret e of
                            Nothing -> putStrLn "NaN"
                            Just x  -> print x
                repl
