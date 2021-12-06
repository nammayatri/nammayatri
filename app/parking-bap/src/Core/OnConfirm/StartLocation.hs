module Core.OnConfirm.StartLocation where

import Beckn.Prelude
import Core.OnConfirm.Descriptor

data StartLocation = StartLocation
  { id :: Text,
    descriptor :: Descriptor,
    gps :: Text
  }
  deriving (Generic, FromJSON)
