{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib.Yudhishthira.SchemaUtils (toInlinedSchemaValue) where

import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.OpenApi hiding (toInlinedSchema)
import Data.OpenApi.Declare (runDeclare)
import qualified Data.Text as T
import Kernel.Prelude

-- | Generates a JSON Schema for a type and inlines all references.
toInlinedSchemaValue :: forall a. ToSchema a => Proxy a -> Value
toInlinedSchemaValue proxy =
  let (defs, sch) = runDeclare (declareSchema proxy) mempty
      jsonSch = toJSON sch
      jsonDefs = toJSON defs
   in resolveRefs jsonDefs jsonSch

resolveRefs :: Value -> Value -> Value
resolveRefs defs val = case val of
  Object km ->
    case KeyMap.lookup "$ref" km of
      Just (String ref) ->
        let refPrefix = "#/components/schemas/"
         in if refPrefix `T.isPrefixOf` ref
              then
                let refName = T.drop (T.length refPrefix) ref
                 in case defs of
                      Object defsKm ->
                        case KeyMap.lookup (Key.fromText refName) defsKm of
                          Just refVal -> resolveRefs defs refVal -- Recursive resolve in case of nested refs
                          Nothing -> val -- Should not happen in a valid schema
                      _ -> val
              else val
      _ -> Object $ KeyMap.map (resolveRefs defs) km
  Array valArr -> Array $ fmap (resolveRefs defs) valArr
  _ -> val
