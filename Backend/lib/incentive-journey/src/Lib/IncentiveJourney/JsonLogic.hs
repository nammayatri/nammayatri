{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

-- | Thin wrapper around repo 'json-logic-hs' (same engine as Yudhishthira 'runLogics'
-- / RideFeedback 'jsonLogicEither'). Supports nested and/or/!/comparisons/var paths.
module Lib.IncentiveJourney.JsonLogic
  ( evaluateJsonLogic,
    lookupVar,
    metricVarsToJsonData,
    collectVarNames,
    metricSnapshotForRule,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Vector as V
import JsonLogic (jsonLogicEither)
import Kernel.Prelude

-- | Evaluate a JsonLogic rule ('conditionLogic') against the metric bag.
-- Match iff the rule returns exactly 'Bool True' (same as RideFeedback).
evaluateJsonLogic :: HM.HashMap Text Int -> A.Value -> Either Text Bool
evaluateJsonLogic vars rule =
  case jsonLogicEither rule (metricVarsToJsonData vars) of
    Left err -> Left $ "JsonLogic eval failed: " <> show err
    Right (A.Bool True) -> Right True
    Right _ -> Right False

-- | Flat bag keys (including dotted ones like @onlineMinutes.total@) → nested JSON
-- so json-logic-hs @{"var":"onlineMinutes.total"}@ path lookup works.
metricVarsToJsonData :: HM.HashMap Text Int -> A.Value
metricVarsToJsonData = A.Object . HM.foldlWithKey' insertVar KM.empty
  where
    insertVar acc key n = insertPath (T.splitOn "." key) (A.Number (fromIntegral n)) acc

    insertPath :: [Text] -> A.Value -> KM.KeyMap A.Value -> KM.KeyMap A.Value
    insertPath [] _ acc = acc
    insertPath [k] v acc = KM.insert (AK.fromText k) v acc
    insertPath (k : ks) v acc =
      let key = AK.fromText k
          child = case KM.lookup key acc of
            Just (A.Object o) -> o
            _ -> KM.empty
       in KM.insert key (A.Object (insertPath ks v child)) acc

lookupVar :: HM.HashMap Text Int -> Text -> Either Text Int
lookupVar vars name =
  case HM.lookup name vars of
    Just n -> Right n
    Nothing -> Left $ "unknown JsonLogic var: " <> name

-- | Variable names referenced by @{"var":"..."}@ (and @{"var":["...", default]}@) in a rule.
collectVarNames :: A.Value -> [Text]
collectVarNames = List.nub . go
  where
    go = \case
      A.Object o -> case KM.toList o of
        [(op, args)]
          | AK.toText op == "var" ->
            case args of
              A.String name -> [name]
              A.Array vals -> case V.toList vals of
                (A.String name : _) -> [name]
                _ -> []
              _ -> []
          | otherwise -> go args
        kvs -> concatMap (go . snd) kvs
      A.Array vals -> concatMap go (V.toList vals)
      _ -> []

-- | Flat JSON object of only the vars used in the rule (for stats UI). Missing bag keys → 0.
metricSnapshotForRule :: Maybe A.Value -> HM.HashMap Text Int -> Maybe A.Value
metricSnapshotForRule Nothing _ = Nothing
metricSnapshotForRule (Just rule) bag =
  let names = collectVarNames rule
   in if null names
        then Nothing
        else
          Just $
            A.Object $
              KM.fromList
                [ (AK.fromText name, A.Number (fromIntegral (HM.lookupDefault 0 name bag)))
                  | name <- names
                ]
