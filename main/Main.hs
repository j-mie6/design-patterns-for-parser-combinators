module Main where

import Interpreter ( eval )
import Parser ( parseExpr )

fromEither :: (e -> a) -> Either e a -> a
fromEither err = either err id

calc :: String -> (String -> Int) -> Int
calc e = eval (fromEither error (parseExpr e))

main :: IO ()
main = do
  putStrLn "This is the Really Cool Calculator (from Really Cool Calculator Inc)"
  putStrLn "Enter an equation, variables x1, xy, and z are available"
  eq <- getLine
  print $ calc eq ctx
  where
    ctx :: String -> Int
    ctx "x1" = 7
    ctx "xy" = 2
    ctx "z"  = 42
    ctx v    = error $ "variable " ++ show v ++ " not in scope"
