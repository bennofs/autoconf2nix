{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Interpreter where

import Control.Monad.State
import Control.Lens
import ShellCheck.AST
import Data.Map (Map)
import Data.Foldable
import Data.Maybe
import Data.List

import qualified Data.Map as Map

import Shell

newtype Pattern = Pattern String
  deriving (Eq, Show)

newtype Output = Output (Map Int String)
  deriving (Eq, Show)

instance Monoid Output where
  mempty = Output mempty
  mappend (Output a) (Output b) = Output (Map.unionWith mappend a b)

data Script a
  = Command FilePath [String] ShellState
  | If Expr Expr Expr
  | Match Pattern Expr
  | Print Output
  | Return Bool Int
  | Pipe Expr Expr
  | Depend (Expr a) (a -> Expr a)
  deriving (Eq, Show)

eval :: MonadState ShellState m => Token -> m Expr
eval token = case token of
  T_Annotation _id _annotations next -> eval next
  T_Pipeline _id [] [next] -> eval next
  T_Pipeline _id [next] [] -> eval next
  T_Redirecting _id [] next -> eval next
  T_NormalWord _id tokens -> T_NormalWord _id tokens -> do


  T_Script _id _interpreter nexts -> Sequence <$> mapM eval nexts
  T_SimpleCommand _id as tokens -> do
    cmd <- mapM (fmap expectString . eval) tokens
    s <- discardState $ evalAssignments as >>= setVariables >> get
    case cmd of
      [] -> Return False mempty 0 <$ put s
      (exe : arguments) -> execCommand exe arguments
  t -> error $ "unhandled token: " ++ show t

evalAssignments :: MonadState ShellState m => [Token] -> m [(AssignmentMode, String, String)]
evalAssignments = mapM $ \token -> case token of
  T_Assignment _id mode name Nothing value ->
    (,,) <$> pure mode <*> pure name <*> discardState (eval value <&> expectString)

expectString :: Expr -> String
expectString (Literal s) = s
expectString t = error $ "expected string literal: " ++ show t

setVariables :: MonadState ShellState m => [(AssignmentMode, String, String)] -> m ()
setVariables = mapM_ $ \(mode, name, value) ->
  modifying (stateVariables . at name . non mempty) $ case mode of
    Append -> variableValue <>~ value
    Assign -> variableValue .~  value

exportVariables :: MonadState ShellState m => Bool -> [String] -> m ()
exportVariables export = mapM_ $ \name -> stateVariables . ix name . variableExported .= export

execCommand :: MonadState ShellState m => FilePath -> [String] -> m Expr
execCommand "export" = builtinExport
execCommand x = error $ "exec unknown command: " ++ show x

validIdentifier :: String -> Bool
validIdentifier "" = False
validIdentifier (x:xs) = x `elem` base && all (`elem` base ++ digits) xs
 where
  digits = ['0'..'9']
  base = ['a'..'z'] ++ ['A'..'Z'] ++ "_"

builtinExport :: MonadState ShellState m => [String] -> m Expr
builtinExport args = do
  setVariables assignments
  exportVariables export allNames
  return $ case invalid of
    [] -> Return False mempty 0
    _  -> Return False mempty 1
 where
  (flags, args') = over _1 (concatMap tail) . span ("-" `isPrefixOf`) $ args
  export = not $ 'n' `elem` flags

  (invalid, names, assignments) = foldMap parseAssignment args'
  allNames = names ++ map (view _2) assignments

  parseAssignment x
    | not (validIdentifier name) = ([name], [], [])
    | Just m <- mode = ([], [], [(m, name, value)])
    | otherwise = ([], [name], [])
   where
    (mode, name, value) = case break (== '=') x of
      (n, "") -> (Nothing, n, "")
      (n, '=':v) -> case reverse n of
        ('+':n') -> (Just Append, reverse n', v)
        _           -> (Just Assign, n, v)
      _ -> error "invalid break result. should be impossible"
