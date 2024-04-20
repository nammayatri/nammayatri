{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.DriverInactiveFCM where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Environment
import EulerHS.Prelude
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.Notifications

data DriverInactiveFCMReq = DriverInactiveFCMReq
  { rideId :: Id DRide.Ride,
    driverId :: Id DP.Person
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

driverInactiveFCM :: DriverInactiveFCMReq -> Flow APISuccess
driverInactiveFCM req = do
  let driverId = req.driverId
      rideId = req.rideId
  ride <- QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  pingDriver person
  _merchantId <- fromMaybeM (InternalError "Ride does not have a merchantId") $ ride.merchantId
  pure Success
  where
    pingDriver driver = do
      case driver.deviceToken of
        Just token -> notifyDevice driver.merchantOperatingCityId FCM.TRIGGER_SERVICE "You were inactive" "Please check the app" driver (Just token)
        Nothing -> log INFO $ "Active drivers with no token" <> show driver.id
