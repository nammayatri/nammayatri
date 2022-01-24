module Core.OnConfirm.OnConfirmMessage where

import Beckn.Prelude
import Core.Provider

data State
  = Active
  | CANCELLED
  deriving (Generic, ToJSON, FromJSON, Show)

{-
data OnConfirmMessage = OnConfirmMessage
  { id :: Text,
    state :: State,
    provider :: Provider,
    items :: ,
    fulfillment ,
    quote ,
    payment :: Payment
  }
  deriving (Generic, Show, ToJSON, FromJSON)
  -}
