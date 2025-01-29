{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Lib.Yudhishthira.Types.ConfigPilot where

import Data.Aeson
import Kernel.Prelude

data Config a = Config
  { config :: a,
    extraDimensions :: Maybe Value,
    identifier :: Int
  }
  deriving (Eq, Generic, ToJSON, FromJSON, Show, Ord)

data Person
