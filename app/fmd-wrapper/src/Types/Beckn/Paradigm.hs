module Types.Beckn.Paradigm where

import Beckn.Utils.Example
import EulerHS.Prelude
import Types.Beckn.Image

data Paradigm = Paradigm
  { _id :: Text,
    _descriptor :: ParadigmDescriptor,
    _policy_id :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON Paradigm where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Paradigm where
  toJSON = genericToJSON stripLensPrefixOptions

data ParadigmDescriptor = ParadigmDescriptor
  { _id :: Text,
    _name :: Maybe Text,
    _sym :: Maybe Text,
    _short_desc :: Maybe Text,
    _long_desc :: Maybe Text,
    _images :: [Image],
    _audio :: Maybe Text,
    _3d_render :: Maybe Text,
    -- FIXME: code field name clashes with the one from Core.Descriptor
    -- We have assumed the domain one here takes precedence
    _code :: Text
  }
  deriving (Generic, Show)

instance FromJSON ParadigmDescriptor where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON ParadigmDescriptor where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example ParadigmDescriptor where
  example =
    ParadigmDescriptor
      { _id = idExample,
        _name = Nothing,
        _sym = Nothing,
        _short_desc = Nothing,
        _long_desc = Nothing,
        _images = [],
        _audio = Nothing,
        _3d_render = Nothing,
        _code = "SINGLE-PICKUP-SINGLE-DROP"
      }

instance Example Paradigm where
  example =
    Paradigm
      { _id = idExample,
        _descriptor = example,
        _policy_id = Nothing
      }
