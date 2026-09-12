{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Hand-written handlers for the direct-dashboard dynamic-logic routes whose
-- authorization spans merchants.
--
-- provider-dashboard checked the caller's merchant_access for every merchant and
-- city in a bulk rollout before forwarding it. The application server has no
-- view of that table and does no such check, so serving the route directly has
-- to do it here or one operator could roll logic out to any merchant.
module Domain.Action.DashboardAuth.Management.NammaTag
  ( postNammaTagAppDynamicLogicBulkUpsertLogicRollout,
  )
where

import qualified Domain.Action.Dashboard.Management.NammaTag
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified Lib.Yudhishthira.Types
import Tools.Auth.DashboardUserAuth

-- | Rolls out only to the merchant/city pairs the caller holds access for, and
-- reports the rest as failures -- the shape provider-dashboard returned.
postNammaTagAppDynamicLogicBulkUpsertLogicRollout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Lib.Yudhishthira.Types.BulkLogicRolloutReq -> Environment.Flow Lib.Yudhishthira.Types.BulkLogicRolloutResult)
postNammaTagAppDynamicLogicBulkUpsertLogicRollout a4 a3 a2 a1 = do
  authorised <- Kernel.Prelude.forM a1.merchantsAndCities $ \entry -> do
    mbAccess <- requestorCityAccess a2 entry.merchantShortId entry.cities
    Kernel.Prelude.pure $ case mbAccess of
      Kernel.Prelude.Nothing ->
        ([], [Lib.Yudhishthira.Types.BulkRolloutCityFailure entry.merchantShortId city "Merchant not found." | city <- entry.cities])
      Kernel.Prelude.Just (allowedCities, deniedCities) ->
        ( [Lib.Yudhishthira.Types.MerchantCitiesEntry entry.merchantShortId allowedCities | not (null allowedCities)],
          [Lib.Yudhishthira.Types.BulkRolloutCityFailure entry.merchantShortId city "You have no access to this operation." | city <- deniedCities]
        )
  let allowedEntries = concatMap fst authorised
      deniedFailures = concatMap snd authorised
  if null allowedEntries
    then Kernel.Prelude.pure (Lib.Yudhishthira.Types.BulkLogicRolloutResult [] deniedFailures)
    else do
      result <-
        Domain.Action.Dashboard.Management.NammaTag.postNammaTagAppDynamicLogicBulkUpsertLogicRollout a4 a3 $
          Lib.Yudhishthira.Types.BulkLogicRolloutReq allowedEntries a1.rollout
      Kernel.Prelude.pure (Lib.Yudhishthira.Types.BulkLogicRolloutResult result.succeeded (result.failures <> deniedFailures))
