{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Fulfillment where

import Beckn.Types.Core.Location
import Beckn.Types.Core.Person
import EulerHS.Prelude

data Fulfillment = Fulfillment
  { person :: Person,
    location :: Location,
    form :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)
