module Main where

import Fakr
{-
main :: IO ()
main = do
  putStrLn "BF Interpreter 0.0.1"
  help
  repl defaulBFState
-}

main :: IO ()
main = do
  let f (Right x) = x
  let h = f $ parseCode helloWorld
  mapM_ print $ refactor $ apply $ h


