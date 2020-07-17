{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Mobility.Driver where

import Beckn.Types.Core.Person
import Beckn.Utils.Common
import Data.Text
import EulerHS.Prelude

data Driver = Driver
  { descriptor :: Person,
    experience :: Maybe Experience
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance Example Driver where
  example =
    Driver
      { descriptor = example,
        experience = example
      }

data Experience = Experience
  { label :: Text,
    value :: Text,
    unit :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance Example Experience where
  example =
    Experience
      { label = "Taxi driver",
        value = "5",
        unit = "stars"
      }
