module Beckn.Types.Core.Migration.Person (Person (..)) where

import Beckn.Types.Core.Migration.Image (Image)
import Beckn.Types.Core.Migration.Name (Name)
import Beckn.Types.Core.Migration.Tags (Tags)
import EulerHS.Prelude

data Person = Person
  { name :: Maybe Name,
    image :: Maybe Image,
    dob :: Maybe Text, -- format: date
    gender :: Maybe Text,
    cred :: Maybe Text,
    tags :: Maybe Tags
  }
  deriving (Eq, Generic, FromJSON, ToJSON, Show)
