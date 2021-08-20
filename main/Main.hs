module Main where

import Interpreter ( eval )
import Parser ( parseExpr )

calc :: String -> (String -> Int) -> Int
calc e = eval (parseExpr e)

main :: IO ()
main = do
  putStrLn "This is the Really Amazing Calculator"
  putStrLn "Enter an equation, variables x1, xy, and z are available"
  eq <- getLine
  print $ calc eq ctx
  where
    ctx :: String -> Int
    ctx "x1" = 7
    ctx "xy" = 2
    ctx "z"  = 42
    ctx v    = error $ "variable " ++ show v ++ " not in scope"