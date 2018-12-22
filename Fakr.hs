{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Fakr where
--(
--  repl,
--  defaulBFState,
--  help,
--  BFExpr'
--) where


import Control.Monad
import Control.Monad.Trans.State
import Data.Char
import System.IO


import qualified Control.Monad.State as S
import qualified Data.List.Zipper as Z
import Text.ParserCombinators.Parsec

data BFExpr = Increment | Decrement
            | GoForward | GoBackward
            | Input     | Output
            | Loop [BFExpr] deriving (Show, Eq)

type Args = [BFExpr]
type Scope =  [(Int, [BFExpr])]


data Refactored =
    RF Int BFExpr
  | RFL Int [Refactored]
  deriving Show

data UserChoice = Quit
                | Reset
                | Help
                | Inspect
                | Verbose Bool
                | Code [BFExpr]
                | Import FilePath deriving Show

type BFState = Z.Zipper Integer

-- Parsing stuff
symbols :: String
symbols = "+-<>,."

instructions :: [BFExpr]

instructions = [Increment, Decrement, GoBackward, GoForward, Input, Output]

parseIntruction :: Parser [BFExpr]
parseIntruction =
  many $
     foldl1 (<|>) (fn:
                 zipWith (\a b -> char a >> return b) symbols instructions)
  where
    fn :: Parser BFExpr
    fn = char '[' >> parseIntruction >>= \ins -> char ']' >> return (Loop ins)


parseCode :: String -> Either ParseError [BFExpr]
parseCode = parse parseIntruction "code.bf"

generiqparse :: String -> UserChoice -> Parser UserChoice
generiqparse str out = do
  _ <- char ':'
  _ <- string str
  pure out

parseVerbose, parseHelp, parseInspect, parseQuit, parseReset, parseImport :: Parser UserChoice
parseQuit    = generiqparse "quit"    Quit
parseInspect = generiqparse "inspect" Inspect
parseReset   = generiqparse "reset"   Reset
parseHelp    = generiqparse "help"    Help

parseVerbose = do
  _ <- char ':'
  _ <- string "verbose"
  spaces
  opt <- try $ string "on" <|> try (string "off")
  pure $ Verbose $ case opt of
    "on" -> True
    "off" -> False
    _    -> error "Invalid option"


parseImport = do
  _ <- char ':'
  _ <- string "import"
  spaces
  many (noneOf []) >>= pure . Import

parseBFCode = parseIntruction >>= pure . Code

parseUserChoice = parse (
                   try parseQuit
               <|> try parseReset
               <|> try parseInspect
               <|> try parseHelp
               <|> try parseImport
               <|> try parseVerbose
               <|> parseBFCode
              ) ""

helloWorld :: String
helloWorld = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."
applyOnCursor' :: (Num a, S.MonadState (Z.Zipper b) m) => (b -> a -> b)
                                                       -> m ()
applyOnCursor' func =
  S.modify $ \ss ->
     let value = func (Z.cursor ss) 1
     in Z.insert value $ Z.delete ss

checkMove :: (BFState -> BFState)
          -> Bool
          -> StateT BFState IO ()
checkMove fn flag = do
  st1 <- S.get
  S.modify fn
  st2 <- S.get
  when (st1 == st2)$ do
    S.modify (extendMemory flag)
    S.modify fn

--lambdaExpr :: BFExpr -> Args -> Scope -> BFExpr
lambdaExpr _ _ _ = do
  putStr " "
  return ()

runCommands' :: Foldable t => t BFExpr -> StateT BFState IO BFState
runCommands' cs = mapM_ runCommand' cs >> S.get

runCommand' :: BFExpr -> StateT BFState IO ()
runCommand' Increment  = applyOnCursor' (+)
runCommand' Decrement  = applyOnCursor' (-)
runCommand' GoForward  = checkMove Z.right True
runCommand' GoBackward = checkMove Z.left False
runCommand' Input      = S.liftIO getLine >>= applyOnCursor' . (\c _ _ -> read c)
runCommand' Output     = get >>= S.liftIO
                              . putStr
                              . (:[])
                              . chr
                              . fromInteger
                              . Z.cursor

runCommand' (Loop cod) = do
  mapM_ runCommand' cod
  st <- S.get
  S.liftIO . putStr . fromBS $ st
  unless (Z.cursor st == 0) $
    runCommand' (Loop cod)

apply :: [BFExpr] -> [[BFExpr]]
apply ins =
  let (collected, it) = popWhile (== (ins !! 0)) ins
  in if null it then
    [collected]
    else
      [collected] ++ (apply it)
  where
    popWhile :: Eq a
            => (a -> Bool)
            -> [a]
            -> ([a], [a])
    popWhile fn xs =
      case xs of
        [] -> ([], [])
        [x] -> if fn x then ([x], []) else ([], [])
        (x:xs') ->
          if fn x then
            let (ps, rst) = popWhile fn xs'
            in (x:ps, rst)
          else
            ([], x:xs')

refactor :: [[BFExpr]] -> [Refactored]
refactor = map rewriter
  where
    rewriter x =
         let size = length x
             item = head x
         in case item of
           (Loop content) -> RFL size $ refactor $ apply content
           _              -> RF size item

ppHW :: String -> IO ()
ppHW = mapM_ print
        . refactor
        . apply
        . (\(Right code) -> code)
        . parseCode

fromBS :: (Show a) => Z.Zipper a
                   -> String
fromBS = (++ "\n")
      . concat
      . Z.toList
      . rewrite
  where
    rewrite (Z.Zip _ []) = Z.Zip [] []
    rewrite (Z.Zip u (x:xs))
      = Z.Zip (map show u) (("<" ++ show x ++ ">"):map show xs)

extendMemory :: Bool
             -> BFState
             -> BFState
extendMemory True  (Z.Zip l r) = Z.Zip l (r ++ replicate 20 0)
extendMemory False (Z.Zip l r) = Z.Zip (l ++ replicate 20 0) r

defaulBFState :: Z.Zipper Integer
defaulBFState = Z.fromList $ replicate 20 0


-- Memory usage

help :: IO ()
help = do
  putStrLn "\n:help  for help"
  putStrLn ":quit  for exit"
  putStrLn ":inspect to show the current state of memory"
  putStrLn ":reset to the initial state of the memory"
  putStrLn ":import file execute a code from a file\n"



repl :: Z.Zipper Integer
     -> IO ()
repl initial = do
  hPutStr stdout "BF> "
  supply <- getLine
  case parseUserChoice supply of
    Left _ -> putStrLn "Invalid input" >> repl initial
    Right code ->
      case code of
        Quit -> do
          putStrLn "Good by :)"
        Help -> do
          help
          repl initial
        Reset -> repl defaulBFState
        Inspect -> do
          putStr $ fromBS initial
          repl initial
        Import file -> do
          content <- parseCode <$> readFile file
          print content
          case content of
            Left _ -> do
              putStrLn ""
            Right ast -> do
              repl . fst =<< runStateT (runCommands' ast) initial
        Code [] -> do
          putStrLn "Syntax Error"
          repl initial
        Code ins -> do
          repl =<< execute ins

execute :: [BFExpr]
        -> IO BFState
execute ins = do
      (st, _) <- runStateT (runCommands' ins) defaulBFState
      putStrLn $ fromBS st
      return st
