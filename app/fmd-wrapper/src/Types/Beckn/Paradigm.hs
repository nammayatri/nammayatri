module Types.Beckn.Paradigm where

import Beckn.Utils.Example
import Beckn.Utils.JSON
import EulerHS.Prelude hiding (id)
import Types.Beckn.Image

data Paradigm = Paradigm
  { id :: Text,
    descriptor :: ParadigmDescriptor,
    policy_id :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON Paradigm where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Paradigm where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data ParadigmDescriptor = ParadigmDescriptor
  { id :: Text,
    name :: Maybe Text,
    sym :: Maybe Text,
    short_desc :: Maybe Text,
    long_desc :: Maybe Text,
    images :: [Image],
    audio :: Maybe Text,
    _3d_render :: Maybe Text,
    -- FIXME: code field name clashes with the one from Core.Descriptor
    -- We have assumed the domain one here takes precedence
    code :: Text
  }
  deriving (Generic, Show)

instance FromJSON ParadigmDescriptor where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON ParadigmDescriptor where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example ParadigmDescriptor where
  example =
    ParadigmDescriptor
      { id = idExample,
        name = Nothing,
        sym = Nothing,
        short_desc = Nothing,
        long_desc = Nothing,
        images = [],
        audio = Nothing,
        _3d_render = Nothing,
        code = "SINGLE-PICKUP-SINGLE-DROP"
      }

instance Example Paradigm where
  example =
    Paradigm
      { id = idExample,
        descriptor = example,
        policy_id = Nothing
      }
