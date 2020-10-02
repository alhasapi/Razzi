module Types where

import qualified Data.List.Zipper as Z

data BFExpr = Increment | Decrement
            | GoForward | GoBackward
            | Input     | Output
            | Loop [BFExpr] deriving (Show, Eq)


data UserChoice = Quit
                | Reset         | Help
                | Inspect       | Verbose Bool
                | Mode Bool
                | SlowedExecution (Either NotDelayed (Int, [BFExpr]))
                | Code [BFExpr] | Import FilePath
                deriving (Show, Eq)


data Opt = OptHelp | File FilePath
data NotDelayed = NotDelayed deriving (Show, Eq)


type BFState = Z.Zipper Integer
