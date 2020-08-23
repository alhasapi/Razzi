{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}

module Fakr (
   repl,
   defaulBFState,
   help,
   banner,
   parseOpt,
   parseCode,
   modish,
   execute,
   animate,
   Opt(..)
) where


import           Control.Concurrent
import           Control.Monad
import qualified Control.Monad.State           as S
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import           Data.Char
import qualified Data.List.Zipper              as Z
import           System.IO
import           Text.ParserCombinators.Parsec

data BFExpr = Increment | Decrement
            | GoForward | GoBackward
            | Input     | Output
            | Loop [BFExpr] deriving (Show, Eq)

data NotDelayed = NotDelayed deriving (Show, Eq)

data UserChoice = Quit
                | Reset         | Help
                | Inspect       | Verbose Bool
                | Mode Bool
                | SlowedExecution (Either NotDelayed (Int, [BFExpr]))
                | Code [BFExpr] | Import FilePath
                deriving (Show, Eq)

data Opt = OptHelp | File FilePath

data Vals :: (* -> * -> *) where
  Ask :: (Eq a) => a -> Vals a Int
  Scatter :: (FromJSON b, ToJSON b, Eq k, Show k)  => [(k, b)]
                                                   -> (b, Int)
                                                   -> [b]
                                                   -> Vals Double b
  Load :: (Traversable a, S.MonadIO a, Foldable a) => Int
                                                   -> [a Int]
                                                   -> Vals Int Double

type BFState = Z.Zipper Integer

class FromJSON a where
  serialize :: (Show b) => String -> b -> a

class ToJSON a where
  deserialize :: (Show b) => a -> b -> String

-- Parsing stuff
symbols :: String
symbols = "+-<>,."

instructions :: [BFExpr]
instructions = [
  Increment,
  Decrement,
  GoBackward,
  GoForward,
  Input,
  Output
 ]

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
generiqparse str out =
   char ':' >> string str >> pure out

parseVerbose, parseInspect, parseHelp, parseQuit, parseReset, parseImport :: Parser UserChoice
parseQuit    = generiqparse "quit"    Quit
parseInspect = generiqparse "inspect" Inspect
parseReset   = generiqparse "reset"   Reset
parseHelp    = generiqparse "help"    Help

parseQIRH :: Parser UserChoice
parseQIRH = foldl1 (<|>) $ map (\q -> try $ uncurry generiqparse (fn q, q)) [Quit, Inspect, Reset, Help]
  where
    fn = ((\(fs:rst) -> (toLower fs):rst) . show)

parseVerbose = do
  _ <- char ':'
  _ <- string "verbose"
  spaces
  opt <- try (string "on") <|> try (string "off")
  pure $ Verbose $ case opt of
    "on"  -> True
    "off" -> False
    _     -> error "Invalid option"

parseMode = do
  _ <- char ':'
  _ <- string "mode"
  spaces
  opt <- try (string "normal") <|> try (string "slow")
  pure $ Mode $ case opt of
    "normal" -> True
    "slow"   -> False
    _        -> error "Invalid option"

parseImport = do
  _ <- char ':'
  _ <- string "import"
  spaces
  path <- many (noneOf [])
  pure $ Import path

parseBFCode, parseSlowerExecution :: Parser UserChoice
parseSlowerExecution = do
  _ <- char ':'
  _ <- string "slow-exec"
  _ <- spaces
  number <- many (oneOf ['0'..'9']) <|> string ""
  _ <- spaces
  bf <- many (noneOf [])
  pure . SlowedExecution $
    case (parseCode bf, number) of
      (Left _,      _) -> Left NotDelayed
      (Right code, "") -> Right (0 , code)
      (Right code,  _) -> Right (read number , code)


parseBFCode = parseIntruction >>= pure . Code
parseUserChoice :: [Char] -> Either ParseError UserChoice
parseUserChoice = parse (
                   try parseQuit
               <|> try parseSlowerExecution
               <|> try parseReset
               <|> try parseMode
               <|> try parseInspect
               <|> try parseHelp
               <|> try parseImport
               <|> try parseVerbose
               <|> try parseBFCode
              ) ""


parseOptHelp :: Parser Opt
parseOptHelp = string "--help" >> return OptHelp

parseOptFile :: Parser Opt
parseOptFile = do
  _ <- string "--file"
  _ <- spaces
  fileName <- many (noneOf [])
  return $ File fileName

parseOpt :: String -> Either ParseError Opt
parseOpt = parse (try parseOptHelp <|> try parseOptFile) ""

helloWorld :: String
helloWorld = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."
applyOnCursor' :: (Num a, S.MonadState (Z.Zipper b) m) => (b -> a -> b)
                                                       -> m ()
applyOnCursor' func =
  S.modify $ \ss ->
     let value = func (Z.cursor ss) 1
     in Z.insert value $ Z.delete ss


checkMove :: S.MonadState BFState m => (BFState -> BFState)
                                    -> Bool
                                    -> m ()
checkMove fn flag = do
  st1 <- S.get
  case st1 of
    (Z.Zip _ []) ->
      S.modify (extendMemory flag)
    (Z.Zip [] _) ->
      S.modify (extendMemory flag)
    (Z.Zip _ _) ->
      S.modify fn
  S.modify fn

runCommands' :: Foldable t => t BFExpr -> StateT BFState IO BFState
runCommands' cs = mapM_ runCommand' cs >> S.get

runCommand' :: S.MonadIO m => BFExpr
                           -> StateT (Z.Zipper Integer) m ()
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
  --S.liftIO . putStr . fromBS $ st
  unless (Z.cursor st == 0) $
    runCommand' (Loop cod)

applyOnCursor
  :: (Monad m, Show t1, Num t2) =>
     (t1 -> t2 -> t1)
     -> StateT (Z.Zipper t1) (WriterT [String] m) ()
applyOnCursor func = do
  S.modify $ \ss ->
     let value = func (Z.cursor ss) 1
     in Z.insert value $ Z.delete ss
  st1 <- get
  S.lift $ tell [fromBS st1]

checkMove'
  :: Monad m =>
     (Z.Zipper Integer -> Z.Zipper Integer)
     -> Bool -> StateT (Z.Zipper Integer) (WriterT [String] m) ()
checkMove' fn flag = do
  st1 <- S.get
  case st1 of
    (Z.Zip _ []) ->
      S.modify (extendMemory flag) >> S.modify fn
    (Z.Zip _ _) -> S.modify fn
  S.lift $ tell [fromBS st1]

runCommands
  :: (S.MonadIO m, Foldable t) =>
     t BFExpr
     -> StateT
          (Z.Zipper Integer) (WriterT [[Char]] m) (Z.Zipper Integer)
runCommands cs = mapM_ runCommand cs >> S.get
runCommand
  :: S.MonadIO m =>
     BFExpr
     -> StateT (Z.Zipper Integer) (WriterT [[Char]] m) ()
runCommand Increment  = applyOnCursor (+)
runCommand Decrement  = applyOnCursor (-)
runCommand GoForward  = checkMove' Z.right True
runCommand GoBackward = checkMove' Z.left False
runCommand Input      = S.liftIO getLine >>= applyOnCursor' . (\c _ _ -> read c)
runCommand Output     = get >>= S.lift
                              . tell
                              . (:[])
                              . (:['\n'])
                              . chr
                              . fromInteger
                              . Z.cursor
runCommand (Loop cod) = do
  mapM_ runCommand cod
  st <- S.get
  --S.liftIO . putStr . fromBS $ st
  S.lift $ tell [fromBS st]
  unless (Z.cursor st == 0) $
    runCommand (Loop cod)

fromBS :: (Show a) => Z.Zipper a -> String
fromBS = -- (++ "\n") .
       concat
      . Z.toList
      . rewrite
  where
    rewrite (Z.Zip _ []) = Z.Zip [] []
    rewrite (Z.Zip u (x:xs))
      = Z.Zip (map show u) (("<" ++ show x ++ ">"):map show xs)

extendMemory :: Bool -> BFState -> BFState
extendMemory True  (Z.Zip l r) = Z.Zip l (r ++ replicate 20 0)
extendMemory False (Z.Zip l r) = Z.Zip (l ++ replicate 20 0) r

defaulBFState :: Z.Zipper Integer
defaulBFState = Z.fromList $ replicate 20 0

help :: IO ()
help =
  mapM_ putStrLn [
          ":help                     to show this help",
          ":quit                     to exit the REPL",
          ":inspect                  to show the current state of memory",
          ":reset                    to the initial state of the memory",
          ":import <file>            to execute a code from a file",
          ":mode <normal | slow>     to execute the code slowly or normaly code",
          ":slow-exec <delay> <code> to execute the code slowly according to the delay \n"
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
      Left _         ->         putStrLn "Invalid input" >> repl initial mode
      Right commands -> handleOptions commands initial mode
  else
    repl initial mode

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
  putStrLn "Here"
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

{-
--- Compiling NTLC to BF:
  * lambda-abstractions
         - Is a way to construct higher order functions
         - multiple arguments are encoded as curried sequence of lambda-abstractions, each sharing its scope with all subsequent
           inner functions
         - normally standard operations, like arithmetic ops, numbers, must be encoded as lambda-terms, exceptionally, as these
           constructs are native to the target language, enforcing lambda caluli's or Churchian rules on them might introduce,
           irrelevant computational overheads, which is not our aim (for now, at least!). We can consider to wrap them with an
           interface which will make them, at a language level, indistinguishable to real lambda-terms.
        -  Closures
  * variables lookup
        Internally, variables either for curried functions or single argument functions are isomorphic to the way they are
        sequentially arranged on the source language. Which means that, for now, the only accepted variables
        are function arguments.
  * lambda-application
        It is where the hierarchy of deflation processes occurs. A lambda-application can yield either a canonical expression which
        maps directly to some representation into the target language, or it produces another lambda expression.
  * beta-reduction
        It is a path which leads ultimately, by applying rewriting rules, to a canonical expression
  * The Effect Fallacy
        FP is still dwelling inside that ethereal realm of platonic ideas where mathematical structures inhabit.
        Just look at John Backus's Turing-award lecture (Can programming be liberated from the Von-Neumann Style?),
        Effects are often seen and presented as a hideous, nearly develish.
-}
