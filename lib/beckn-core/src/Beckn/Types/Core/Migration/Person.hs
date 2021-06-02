module Beckn.Types.Core.Migration.Person (Person (..)) where

import Beckn.Types.Core.Migration.Image (Image)
import Beckn.Types.Core.Migration.Name (Name)
import Beckn.Types.Core.Migration.Tags (Tags)
import Beckn.Utils.JSON
import EulerHS.Prelude

data Person = Person
  { name :: Name,
    image :: Maybe Image,
    dob :: Maybe Text, -- format: date
    gender :: Maybe Text,
    cred :: Maybe Text,
    tags :: Maybe Tags
  }
  deriving (Eq, Generic, Show)

instance FromJSON Person where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Person where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
