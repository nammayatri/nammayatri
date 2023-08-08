{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.BulkLocUpdate where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Environment
import EulerHS.Prelude
import Kernel.External.Maps.Types
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.LocationUpdates as LocUpd
import qualified Storage.Queries.Ride as QRide

data BulkLocUpdateReq = BulkLocUpdateReq
  { rideId :: Id DRide.Ride,
    driverId :: Id DP.Person,
    loc :: NonEmpty LatLong
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

bulkLocUpdate :: BulkLocUpdateReq -> Flow APISuccess
bulkLocUpdate req = do
  let rideId = req.rideId
      driverId = req.driverId
      loc = req.loc
  ride <- QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  defaultRideInterpolationHandler <- LocUpd.buildRideInterpolationHandler (fromMaybe "" ride.merchantId) False
  _ <- LocUpd.addIntermediateRoutePoints defaultRideInterpolationHandler rideId driverId loc
  pure Success
