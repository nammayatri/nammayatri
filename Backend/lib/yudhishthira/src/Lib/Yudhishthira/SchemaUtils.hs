{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib.Yudhishthira.SchemaUtils (toInlinedSchemaValue) where

import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.OpenApi hiding (toInlinedSchema)
import Data.OpenApi.Declare (runDeclare)
import qualified Data.Set as Set
import qualified Data.Text as T
import Kernel.Prelude

-- | Generates a JSON Schema for a type and inlines all references.
toInlinedSchemaValue :: forall a. ToSchema a => Proxy a -> Value
toInlinedSchemaValue proxy =
  let (defs, sch) = runDeclare (declareSchema proxy) mempty
      jsonSch = toJSON sch
      jsonDefs = toJSON defs
   in resolveRefs jsonDefs Set.empty jsonSch

-- | Inline every @$ref@ in the schema by substituting its definition. @inlining@ tracks the
-- ref names currently being expanded on this path: if a ref is already being inlined (a
-- self-referential / mutually-recursive type, e.g. @FareBreakupNode@ <-> @FareBreakupNodeValue@),
-- the back-edge is left as a plain @$ref@ instead of being expanded forever. Without this guard a
-- recursive type makes the output grow without bound.
resolveRefs :: Value -> Set.Set Text -> Value -> Value
resolveRefs defs inlining val = case val of
  Object km ->
    case KeyMap.lookup "$ref" km of
      Just (String ref) ->
        let refPrefix = "#/components/schemas/"
         in if refPrefix `T.isPrefixOf` ref
              then
                let refName = T.drop (T.length refPrefix) ref
                 in if refName `Set.member` inlining
                      then val -- cycle detected: keep the $ref to break the recursion
                      else case defs of
                        Object defsKm ->
                          case KeyMap.lookup (Key.fromText refName) defsKm of
                            Just refVal -> resolveRefs defs (Set.insert refName inlining) refVal -- Recursive resolve in case of nested refs
                            Nothing -> val -- Should not happen in a valid schema
                        _ -> val
              else val
      _ -> Object $ KeyMap.map (resolveRefs defs inlining) km
  Array valArr -> Array $ fmap (resolveRefs defs inlining) valArr
  _ -> val
