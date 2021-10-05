module Beckn.Utils.Schema where

import Beckn.Utils.Common (recursiveStrip)
import Data.OpenApi.ParamSchema

stripPrefixUnderscoreIfAny :: SchemaOptions
stripPrefixUnderscoreIfAny =
  defaultSchemaOptions
    { fieldLabelModifier = recursiveStrip
    }