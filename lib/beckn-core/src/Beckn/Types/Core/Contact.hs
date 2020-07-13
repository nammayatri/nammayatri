{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Contact where

import Beckn.Utils.Common
import Data.Text
import EulerHS.Prelude

data Contact = Contact
  { email :: Maybe Text,
    mobile :: Maybe Mobile,
    landline :: Maybe LandLine,
    ivr :: [Text]
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance Example Contact where
  example =
    Contact
      { email = Just "nyan.cat@gmail.com",
        mobile = example,
        landline = example,
        ivr = ["some ivr"]
      }

data Mobile = Mobile
  { country_code :: Maybe Text,
    number :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance Example Mobile where
  example =
    Mobile
      { country_code = Just "+9",
        number = Just "19999999999"
      }

data LandLine = LandLine
  { country_code :: Text,
    std_code :: Text,
    number :: Text,
    extension :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance Example LandLine where
  example =
    LandLine
      { country_code = "+9",
        std_code = "",
        number = "123",
        extension = ""
      }
