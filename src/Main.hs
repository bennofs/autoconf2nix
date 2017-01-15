{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import System.IO
import System.Exit
import System.Environment
import System.FilePath
import Control.Exception
import Control.Lens
import ShellCheck.AST
import ShellCheck.Interface
import ShellCheck.Parser
import Data.Data.Lens
import Data.Data
import Data.Map (Map)
import Data.List (mapAccumL)
import Data.Foldable (foldl')
import Data.Maybe
import Control.Monad.State

import qualified Data.Map.Strict as Map

import Shell
import Interpreter

ioInterface :: SystemInterface IO
ioInterface = SystemInterface $
  over (mapped._Left) displayError . try . readFile
 where
  displayError e = show (e :: IOError)

parseConfigure :: FilePath -> IO (Maybe Token)
parseConfigure file = readFile file >>= fmap prRoot . parseScript ioInterface . ParseSpec file

main :: IO ()
main = getArgs >>= parseConfigure . head >>= maybe parseFailed go
 where
  parseFailed = hPutStrLn stderr "Failed to parse configure" >> exitFailure
  go token = print $ evalState (eval token) initialShellState
