module Parser where

import Types
import Data.Char
import Text.ParserCombinators.Parsec


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
parseCode
  = parse parseIntruction "code.bf"

generiqparse :: String -> UserChoice -> Parser UserChoice
generiqparse str out
  = char ':' >> string str >> pure out

parseVerbose, parseInspect, parseHelp, parseQuit, parseReset, parseImport :: Parser UserChoice
parseQuit    = generiqparse "quit"    Quit
parseInspect = generiqparse "inspect" Inspect
parseReset   = generiqparse "reset"   Reset
parseHelp    = generiqparse "help"    Help

parseQIRH :: Parser UserChoice
parseQIRH
  = foldl1 (<|>) $ map (\q -> try $ uncurry generiqparse (fn q, q)) [Quit, Inspect, Reset, Help]
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
