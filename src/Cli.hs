{-# LANGUAGE LambdaCase #-}

module Cli where

import Fakr
import Types
import Parser
import Data.Char
import System.IO
import Control.Monad
import Control.Concurrent
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import qualified Data.List.Zipper as Z


help :: IO ()
help =
  mapM_ putStrLn [
          ":help                     to show this help",
          ":quit                     to exit the REPL",
          ":inspect                  to show the current state of memory",
          ":reset                    to the initial state of the memory",
          ":import <file>            to execute a code from a file",
          ":mode <normal | slow>     to execute the code slowly or normaly code",
          ":slow-exec <delay> <code> to execute the code slowly according to the delay\n"
        ]
repl
  :: Z.Zipper Integer
     -> Bool
     -> IO ()
repl initial mode = do
  hPutStr stdout "~ >> "
  hFlush stdout
  supply <- getLine
  if "" /= supply then
    case parseUserChoice supply of
      Left _         -> putStrLn "Invalid input" >> repl initial mode
      Right commands -> handleOptions commands initial mode
  else
    repl initial mode

defaulBFState :: Z.Zipper Integer
defaulBFState = Z.fromList $ replicate 20 0

handleOptions
  :: UserChoice -> Z.Zipper Integer -> Bool -> IO ()
handleOptions (Verbose True) initial mode
  = putStr (fromBS initial) >> repl initial mode
handleOptions (Verbose False) initial mode
  = repl initial mode
handleOptions (Mode False) initial _
  = repl initial False
handleOptions (Mode True) initial _
  = repl initial True
handleOptions Quit _ _
  = goodBye
handleOptions Help initial mode
  = help >> repl initial mode
handleOptions (SlowedExecution (Right (delay, commands))) initial mode
  = putStrLn "Entering Slow Execution mode" >> execute commands (Right delay) initial >>= flip repl mode
handleOptions (SlowedExecution (Left _)) initial mode
  = repl initial mode
handleOptions Reset  _ mode
  = repl defaulBFState mode
handleOptions Inspect initial mode
  = putStrLn (fromBS initial) >> repl initial mode
handleOptions (Import fileName) initial mode
  = (\case
        Left err ->
          print err
        Right ast ->
            flip repl mode =<< execute ast (modish mode) initial)
                           <$> parseCode
                           <$> readFile fileName >>= void
handleOptions (Code []) initial mode
  = putStrLn "Unkown instruction" >> repl initial mode
handleOptions (Code ins) initial mode
  = execute ins (modish mode) initial >>= flip repl mode

modish :: Bool -> Either NotDelayed Int
modish mode
  = if not mode then Right 50 else Left NotDelayed

execute
  :: [BFExpr]
      -> Either NotDelayed Int
      -> BFState
      -> IO BFState
execute ins delayed_ current
  = case delayed_ of
    Left NotDelayed -> do
      (st, _) <- runStateT (runCommands' ins) current
      putStrLn $ fromBS st
      return st
    Right delay' -> slowExec ins delay' current

putProgress :: String -> IO ()
putProgress s =
  hPutStr stderr $ "\r\ESC[K" ++ s

putStr' :: Int -> String -> IO ()
putStr' delay value = do
  putProgress value
  threadDelay $ delay * 1000

slowExec :: [BFExpr] -> Int -> BFState -> IO BFState
slowExec code delay context = do
  value <- runWriterT (runStateT (runCommands code) context)
  mapM_ (putStr' delay) (snd value)
  putStrLn ""
  return . fst . fst $ value

goodBye :: IO ()
goodBye = animate 58 "good by :)"

animate :: Int -> String -> IO ()
animate time str
  = forM_ (generateAnimatedWords (str ++ " ")) (putStr' time) >> putStrLn ""

generateAnimatedWords
  :: String -> [String]
generateAnimatedWords str' = map (capitalizeN str' limit') [0..(limit' - 1)]
  where
    limit' = length str'
    capitalizeN str limit 0 =
      transform (head str) : slice 1 (limit - 1) str
    capitalizeN str limit n =
      slice 0 (n - 1) str
        ++ [transform (str !! n)]  ++
      slice (n + 1) (limit - 1) str
    slice from to str = map (str !!) [from..to]
    transform x =
      if isLower x then
        toUpper x
      else
        toLower x

banner :: IO ()
banner = mapM_ putStrLn [
            "      ::::::::::   :::     :::    ::: :::::::::",
            "    :+:        :+: :+:   :+:   :+:  :+:    :+: ",
            "    +:+       +:+   +:+  +:+  +:+   +:+    +:+ ",
            "  :#::+::# +#++:++#++: +#++:++    +#++:++#:    ",
            "  +#+      +#+     +#+ +#+  +#+   +#+    +#+   ",
            "#+#      #+#     #+# #+#   #+#  #+#    #+#     ",
            "###      ###     ### ###    ### ###    ###     \n",
            "              In memoriam of Fakrdin razzi (1149-1209)\n"
        ]
