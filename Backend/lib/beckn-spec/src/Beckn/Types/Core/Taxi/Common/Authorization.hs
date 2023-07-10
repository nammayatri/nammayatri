module Beckn.Types.Core.Taxi.Common.Authorization where

import Data.OpenApi hiding (Example, example, name, tags)
import Kernel.Prelude
import Kernel.Utils.JSON (stripPrefixUnderscoreIfAny)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data Authorization = Authorization
  { _type :: Text,
    token :: Text
  }
  deriving (Eq, Generic, Show)

instance ToSchema Authorization where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions stripPrefixUnderscoreIfAny

instance FromJSON Authorization where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Authorization where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
