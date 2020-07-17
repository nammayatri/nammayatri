{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Person where

import Beckn.Types.Core.Contact
import Beckn.Utils.Common
import Data.Text
import EulerHS.Prelude

data Person = Person
  { title :: Text, -- "Mr", "Mrs", "Miss", "Dr"
    first_name :: Text,
    middle_name :: Text,
    last_name :: Text,
    full_name :: Text,
    image :: Maybe Image,
    dob :: Maybe Text,
    gender :: Text, -- male, female
    contact :: Contact
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance Example Person where
  example =
    Person
      { title = "Mr",
        first_name = "John",
        middle_name = "Smith",
        last_name = "",
        full_name = "John Smith",
        image = example,
        dob = Just "28-11-1990",
        gender = "male",
        contact = example
      }

data Image = Image
  { _format :: Text, -- "url", "encoded"
    _data :: Text
  }
  deriving (Generic, Show)

instance FromJSON Image where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Image where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Image where
  example =
    Image
      { _format = "url",
        _data = "https://i.imgur.com/MjeqeUP.gif"
      }
