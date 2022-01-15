module Core.OnStatus
  ( module Core.OnStatus,
    module Reexport,
  )
where

import Core.Order as Reexport

import Data.Aeson
import GHC.Generics

newtype OnStatus = OnStatus
  { order :: Order
  }
  deriving (Generic, ToJSON, FromJSON)