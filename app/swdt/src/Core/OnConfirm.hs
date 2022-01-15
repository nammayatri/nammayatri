module Core.OnConfirm
  ( module Core.OnConfirm,
    module Reexport,
  )
where

import Core.Order as Reexport
import Core.ConfirmError as Reexport

import Data.Aeson
import GHC.Generics

newtype OnConfirmMessage = OnConfirmMessage
  { order :: Order
  }
  deriving (Generic, ToJSON, FromJSON)

newtype OnConfirmErrorMessage = OnConfirmErrorMessage
  { error :: ConfirmError
  }
  deriving (Generic, ToJSON, FromJSON)
  