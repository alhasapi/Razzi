{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}

module Fakr (
   parseOpt,
   parseCode,
   Opt(..),
   BFState,
   BFExpr,
   runCommands',
   runCommands,
   toLower,
   toUpper,
   fromBS
) where


import Types
import Parser
import Data.Char
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import qualified Data.List.Zipper as Z
import qualified Control.Monad.State as S

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
    rewrite (Z.Zip _ [])
      = Z.Zip [] []
    rewrite (Z.Zip [] (x:xs))
      = Z.Zip [] (("<" ++ show x ++ ">"):map show xs)
    rewrite (Z.Zip u (x:xs))
      = Z.Zip (map show u) (("<" ++ show x ++ ">"):map show xs)

extendMemory :: Bool -> BFState -> BFState
extendMemory True  (Z.Zip l r)
  = Z.Zip l (r ++ replicate 20 0)
extendMemory False (Z.Zip l r)
  = Z.Zip (l ++ replicate 20 0) r


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
        In John Backus's Turing-award lecture (Can programming be liberated from the Von-Neumann Style?),
        Effects are often seen and presented as a hideous, nearly devilish.
-}
