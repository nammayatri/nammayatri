module Beckn.Types.Core.Migration.Address (Address (..)) where

import EulerHS.Prelude

data Address = Address
  { _door :: Maybe Text,
    _name :: Maybe Text,
    _building :: Maybe Text,
    _street :: Maybe Text,
    _locality :: Maybe Text,
    _ward :: Maybe Text,
    _city :: Maybe Text,
    _state :: Maybe Text,
    _country :: Maybe Text,
    _area_code :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON Address where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Address where
  toJSON = genericToJSON stripAllLensPrefixOptions
