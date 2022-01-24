module Core.Confirm
  ( module Core.Confirm,
    module Reexport,
  )
where

import Core.Order as Reexport
import Data.Aeson
import GHC.Generics

newtype ConfirmMessage = ConfirmMessage
  { order :: Order
  }
  deriving (Generic, ToJSON, FromJSON)
