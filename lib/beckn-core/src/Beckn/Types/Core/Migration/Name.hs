module Beckn.Types.Core.Migration.Name (Name (..)) where

import Data.Aeson (Value (..), object, withObject, (.:), (.=))
import EulerHS.Prelude hiding ((.=))

data Name
  = Full Text
  | Deconstructed DeconstructedName
  deriving (Show)

instance FromJSON Name where
  parseJSON = withObject "Name" $ \o ->
    (Full <$> o .: "full")
      <|> (Deconstructed <$> parseJSON (Object o))

instance ToJSON Name where
  toJSON (Full name) = object ["full" .= name]
  toJSON (Deconstructed decons) = toJSON decons

data DeconstructedName = DeconstructedName
  { _additional_name :: Maybe Text,
    _family_name :: Maybe Text,
    _given_name :: Maybe Text,
    _call_sign :: Maybe Text,
    _honorific_prefix :: Maybe Text,
    _honorific_suffix :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON DeconstructedName where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON DeconstructedName where
  toJSON = genericToJSON stripLensPrefixOptions
