{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.DeviationAlert
  ( FCMReq (..),
    sendDeviationFCM,
  )
where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as Ride
import Kernel.Beam.Functions (runInReplica)
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBAP as BP
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import Tools.Metrics (CoreMetrics)

data FCMReq = FCMReq
  { rideId :: Id Ride.Ride,
    reason :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

sendDeviationFCM ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasHttpClientOptions r c
  ) =>
  (Id Person.Person, Id DM.Merchant) ->
  FCMReq ->
  m APISuccess.APISuccess
sendDeviationFCM (_, _) FCMReq {..} = do
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  void $ QRide.updateSafetyAlertTriggerCount rideId ride.safetyAlertTriggerCount
  BP.sendAlertToBAP booking ride reason
  pure APISuccess.Success
