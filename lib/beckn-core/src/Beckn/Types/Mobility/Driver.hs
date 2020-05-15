{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Mobility.Driver where

import Beckn.Types.Core.Item
import Beckn.Types.Core.Person
import Data.Generics.Labels
import Data.Text
import EulerHS.Prelude

data Driver = Driver
  { descriptor :: Person,
    experience :: Maybe Experience
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data Experience = Experience
  { label :: Text,
    value :: Text,
    unit :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)
