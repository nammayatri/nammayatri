{-# LANGUAGE DuplicateRecordFields #-}
module Beckn.Types.Mobility.Vehicle where

import Data.Generics.Labels
import Data.Text
import EulerHS.Prelude

data Vehicle = Vehicle
  { category :: Maybe Text, -- "CAR", "MOTORCYCLE", "BICYCLE", "TRUCK", "OTHER"
    capaciity :: Maybe Int,
    make :: Maybe Text,
    model :: Maybe Text,
    size :: Maybe Text,
    variant :: Text,
    color :: Maybe Text,
    energy_type :: Maybe Text, -- "PETROL", "DIESEL", "LPG", "CNG", "EV", "OTHER"
    registration :: Maybe Registration
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data Registration = Registration
  { category :: Text, -- "PERSONAL", "COMMERCIAL", "OTHER"
    number :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)
