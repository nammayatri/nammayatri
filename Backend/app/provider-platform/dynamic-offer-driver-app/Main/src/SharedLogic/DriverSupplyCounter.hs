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
    onRideCountKey,
    recordDriverActiveChange,
    recordOnRideChange,
  )
where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common

onlineCountKey :: Text -> Text
onlineCountKey cityId = "driverSupply:onlineCount:" <> cityId

onRideCountKey :: Text -> Text
onRideCountKey cityId = "driverSupply:onRideCount:" <> cityId

-- | incr/decr throw, so every mutation is wrapped -- a Redis blip must never fail the
-- business flow that triggered it.
bumpCounter :: (Redis.HedisFlow m r, MonadFlow m) => (Text -> Text) -> Bool -> Id DMOC.MerchantOperatingCity -> m ()
bumpCounter mkKey up cityId =
  void $ withTryCatch "driverSupplyCounter" $ (if up then Redis.incr else Redis.decr) (mkKey cityId.getId)

-- | Only moves on a real change: updateDriverModeAndFlowStatus is called on every ride
-- start and end with `active` unchanged, so an unguarded incr/decr would track ride
-- volume, not supply.
recordDriverActiveChange ::
  (Redis.HedisFlow m r, MonadFlow m) =>
  Maybe (Id DMOC.MerchantOperatingCity) ->
  Bool ->
  Bool ->
  m ()
recordDriverActiveChange mbCityId wasActive nowActive =
  when (wasActive /= nowActive) $
    whenJust mbCityId $ bumpCounter onlineCountKey nowActive

-- | Ride start increments, completion/cancellation decrements. Callers must only
-- decrement for a ride that actually started, or the counter drifts below true.
recordOnRideChange :: (Redis.HedisFlow m r, MonadFlow m) => Id DMOC.MerchantOperatingCity -> Bool -> m ()
recordOnRideChange cityId started = bumpCounter onRideCountKey started cityId
