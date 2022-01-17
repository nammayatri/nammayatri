module Core.Search.Message where

import Beckn.Prelude
import Core.Search.Intent

newtype Message = Message
  { intent :: Intent
  }
  deriving (Generic, FromJSON, ToJSON, Show)
