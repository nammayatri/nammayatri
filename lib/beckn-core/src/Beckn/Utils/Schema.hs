module Beckn.Utils.Schema where

import Beckn.Utils.Common (recursiveStrip)
import qualified Data.Aeson as A
import Data.OpenApi.ParamSchema

stripPrefixUnderscoreIfAny :: SchemaOptions
stripPrefixUnderscoreIfAny =
  defaultSchemaOptions
    { fieldLabelModifier = recursiveStrip
    }

untaggedValue :: SchemaOptions
untaggedValue =
  defaultSchemaOptions
    { sumEncoding = A.UntaggedValue
    }