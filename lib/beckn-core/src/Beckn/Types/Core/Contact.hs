{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Contact where

import Data.Text
import EulerHS.Prelude

data Contact = Contact
  { email :: Maybe Text,
    mobile :: Maybe Mobile,
    landline :: Maybe LandLine,
    ivr :: [Text]
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data Mobile = Mobile
  { country_code :: Text,
    number :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data LandLine = LandLine
  { country_code :: Text,
    std_code :: Text,
    number :: Text,
    extension :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)
