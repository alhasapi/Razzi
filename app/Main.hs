{-# LANGUAGE LambdaCase #-}

module Main where

import Cli
import Types
import Parser
import Control.Monad
import System.Environment

main :: IO ()
main = do
  arguments <- concat <$> getArgs
  if ("" == arguments) then do
      -- foldl1 (>>) [banner, help, repl defaulBFState True]
      banner
      help
      repl defaulBFState True
  else
    case parseOpt arguments of
      Left err -> print err
      Right OptHelp ->
        putStrLn "Usage: fakr [--help | --file <file name>]"
      Right (File fname) ->
          (\case
            (Left  err)  -> print err
            (Right ast) -> do
              _ <- execute ast (modish True) defaulBFState
              return ()) <$> parseCode
                         <$> readFile fname >>= void
