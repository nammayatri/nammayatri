{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Person (Person (..)) where

import Beckn.Types.Core.Migration.Image (Image)
import Beckn.Types.Core.Migration.Name (Name)
import Beckn.Types.Core.Migration.Tags (Tags)
import Beckn.Utils.JSON (deriveJSON)
import Data.Time (UTCTime)
import EulerHS.Prelude

data Person = Person
  { _name :: Maybe Name,
    _image :: Maybe Image,
    _dob :: Maybe UTCTime, -- FIXME: format: date
    _gender :: Maybe Text,
    _cred :: Maybe Text,
    _tags :: Maybe [Tags] -- FIXME: probably needs to be just Maybe Tags
  }
  deriving (Generic, Show)

deriveJSON ''Person 'stripLensPrefixOptions
