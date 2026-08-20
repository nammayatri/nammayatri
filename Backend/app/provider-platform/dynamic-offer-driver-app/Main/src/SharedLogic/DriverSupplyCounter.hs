{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Dispatch-eligible driver count per operating city, held in Redis.
-- Deliberately a leaf: the writers of driver_information.active live in the storage
-- and consumer layers, and this lets them maintain the counter without pulling in the
-- publisher and its metrics/ClickHouse dependencies.
module SharedLogic.DriverSupplyCounter
  ( onlineCountKey,
    recordDriverActiveChange,
  )
where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common

onlineCountKey :: Text -> Text
onlineCountKey cityId = "driverSupply:onlineCount:" <> cityId

-- | Only moves on a real change: updateDriverModeAndFlowStatus is called on every ride
-- start and end with `active` unchanged, so an unguarded incr/decr would track ride
-- volume, not supply. incr/decr throw (unlike sAddExp), hence withTryCatch.
recordDriverActiveChange ::
  (Redis.HedisFlow m r, MonadFlow m) =>
  Maybe (Id DMOC.MerchantOperatingCity) ->
  Bool ->
  Bool ->
  m ()
recordDriverActiveChange mbCityId wasActive nowActive =
  when (wasActive /= nowActive) $
    whenJust mbCityId $ \cityId ->
      void $ withTryCatch "driverSupplyCounter" $ (if nowActive then Redis.incr else Redis.decr) (onlineCountKey cityId.getId)
