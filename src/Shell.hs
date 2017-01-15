{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Shell where

import Control.Monad.State
import Control.Lens
import Data.Map (Map)

data ShellVariable = ShellVariable
  { _variableExported :: !Bool
  , _variableValue    :: !String
  } deriving (Eq, Show)
makeLenses ''ShellVariable

instance Monoid ShellVariable where
  mempty = ShellVariable False mempty
  mappend (ShellVariable export value) (ShellVariable export' value')
    = ShellVariable (export || export') (mappend value value')

data ShellState = ShellState
  { _stateVariables :: !(Map String ShellVariable)
  , _stateFiles     :: !(Map FilePath String)
  } deriving (Eq, Show)
makeLenses ''ShellState

initialShellState :: ShellState
initialShellState = ShellState mempty mempty

discardState :: MonadState s m => m a -> m a
discardState m = get >>= \saved -> m <* put saved
