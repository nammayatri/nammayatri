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
  )
where

import qualified Data.Aeson as A
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
      case JsonLogic.jsonLogicEither (cohort.eligibilityJsonLogic) ctx of
        Right (A.Bool True) -> Just cohort.id
        Right (A.Number n) | n /= 0 -> Just cohort.id
        _ -> Nothing
