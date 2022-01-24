module Core.OnCansel
  ( module Core.OnCansel,
    module Reexport,
  )
where

import Core.Order as Reexport
import Data.Aeson
import GHC.Generics

newtype OnCanselMessage = OnCanselMessage
  { order :: Order
  }
  deriving (Generic, ToJSON, FromJSON)
