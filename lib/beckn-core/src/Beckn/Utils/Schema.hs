module Beckn.Utils.Schema where

import Data.OpenApi.ParamSchema

stripPrefixUnderscoreIfAny :: SchemaOptions
stripPrefixUnderscoreIfAny =
  defaultSchemaOptions
    { fieldLabelModifier = recursiveStrip
    }
  where
    recursiveStrip = \case
      ('_' : xs) -> recursiveStrip xs
      a -> a