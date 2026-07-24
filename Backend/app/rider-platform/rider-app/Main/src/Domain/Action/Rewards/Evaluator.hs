{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Action.Rewards.Evaluator
  ( evaluateCohorts,
    evaluateCohortsPure,
    evalCohortLogic,
    interpretEligibility,
    collectVarNames,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Vector as V
import qualified Domain.Types.RewardCohort as DRC
import qualified JsonLogic
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common

evaluateCohorts ::
  (MonadFlow m) =>
  A.Value ->
  [DRC.RewardCohort] ->
  m [Id DRC.RewardCohort]
evaluateCohorts ctx cohorts = pure $ evaluateCohortsPureImpl ctx cohorts

evaluateCohortsPure :: (Monad m) => A.Value -> [DRC.RewardCohort] -> m [Id DRC.RewardCohort]
evaluateCohortsPure ctx cohorts = pure $ evaluateCohortsPureImpl ctx cohorts

evaluateCohortsPureImpl :: A.Value -> [DRC.RewardCohort] -> [Id DRC.RewardCohort]
evaluateCohortsPureImpl ctx = mapMaybe match
  where
    match cohort =
      case evalCohortLogic cohort.eligibilityJsonLogic ctx of
        Right (_, True) -> Just cohort.id
        _ -> Nothing

-- | How a raw JSON-logic result is interpreted as an eligibility verdict.
-- Truthy = a @true@ boolean or any non-zero number; everything else is not
-- eligible. Kept as the single definition so the dashboard validation tool and
-- the production evaluator can never disagree.
interpretEligibility :: A.Value -> Bool
interpretEligibility (A.Bool True) = True
interpretEligibility (A.Number n) = n /= 0
interpretEligibility _ = False

-- | Evaluate an eligibility JSON logic against a context, returning the raw
-- evaluated value alongside the eligibility verdict, or the evaluation error.
-- Shared by 'evaluateCohortsPureImpl' and the dashboard validation handler.
--
-- @JsonLogic.jsonLogicEither :: Value -> Value -> Either SomeException Value@,
-- so the 'Left' is a 'SomeException'; we normalize it to 'String' via 'show' to
-- keep a simple, comparable error type.
evalCohortLogic :: A.Value -> A.Value -> Either String (A.Value, Bool)
evalCohortLogic logic ctx =
  case JsonLogic.jsonLogicEither logic ctx of
    Left e -> Left (show e)
    Right v -> Right (v, interpretEligibility v)

-- | Collect every field name referenced via @{"var": ...}@ in a JsonLogic rule.
-- Handles the string form @{"var":"x"}@, the array form @{"var":["x", default]}@
-- (recursing into the default expression), and recurses through all nested
-- operators/arrays. Used to reject logics that reference fields outside the
-- typed context. Whole-context references (@{"var":""}@) surface as an empty
-- string and are filtered by the caller.
collectVarNames :: A.Value -> [Text]
collectVarNames = \case
  A.Object o -> case AKM.toList o of
    [("var", varArg)] -> varArgNames varArg
    kvs -> concatMap (collectVarNames . snd) kvs
  A.Array a -> concatMap collectVarNames (V.toList a)
  _ -> []
  where
    varArgNames (A.String s) = [s]
    varArgNames (A.Array a) = case V.toList a of
      (A.String s : rest) -> s : concatMap collectVarNames rest
      xs -> concatMap collectVarNames xs
    varArgNames v = collectVarNames v
