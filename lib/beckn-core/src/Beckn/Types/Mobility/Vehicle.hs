{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Mobility.Vehicle where

import Beckn.Utils.Common
import Data.Text
import EulerHS.Prelude

data Vehicle = Vehicle
  { category :: Maybe Text, -- "CAR", "MOTORCYCLE", "BICYCLE", "TRUCK", "AUTO-RICKSHAW"
    capacity :: Maybe Int,
    make :: Maybe Text,
    model :: Maybe Text,
    size :: Maybe Text,
    variant :: Text,
    color :: Maybe Text,
    energy_type :: Maybe Text, -- "PETROL", "DIESEL", "LPG", "CNG", "EV", "OTHER"
    registration :: Maybe Registration
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance Example Vehicle where
  example =
    Vehicle
      { category = Just "CAR",
        capacity = Just 5,
        make = Just "Logan",
        model = Just "Renault",
        size = Just "small",
        variant = "X10",
        color = Just "indigo",
        energy_type = Just "PETROL",
        registration = example
      }

data Registration = Registration
  { category :: Text, -- "PERSONAL", "COMMERCIAL", "OTHER"
    number :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance Example Registration where
  example =
    Registration
      { category = "PERSONAL",
        number = "AA-12-BB-3456"
      }
