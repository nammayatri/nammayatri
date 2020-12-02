{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Person (Person (..)) where

import Beckn.Types.Core.Migration.Image (Image)
import Beckn.Types.Core.Migration.Name (Name)
import Beckn.Types.Core.Migration.Tags (Tags)
import Data.Aeson.TH (deriveJSON)
import EulerHS.Prelude

data Person = Person
  { _name :: Maybe Name,
    _image :: Maybe Image,
    _dob :: Maybe Text, -- format: date
    _gender :: Maybe Text,
    _cred :: Maybe Text,
    _tags :: [Tags] -- Fix after that https://github.com/beckn/protocol-specifications/pull/61
  }
  deriving (Generic, Show)

deriveJSON stripLensPrefixOptions ''Person
