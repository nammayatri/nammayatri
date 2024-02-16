{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.RideRoute
  ( rideRoute,
  )
where

import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Domain.Types.Ride
import Domain.Types.RideRoute
import Kernel.Prelude
import Kernel.Storage.Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Ride

rideRoute :: (EncFlow m r, HedisFlow m r) => Id Ride -> (Id Person.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> m RouteInfo
rideRoute rideId (_, _, _) = do
  let key = searchRequestKey (getId rideId)
  safeGet key >>= fromMaybeM (RideDoesNotExist $ getId rideId)
