{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.ExotelEndRide
  ( AckResp,
    callBasedEndRide,
  )
where

import qualified Domain.Action.UI.Ride.EndRide as EndRide
import Domain.Types.Merchant
import qualified Domain.Types.Person as DP
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import qualified Kernel.Storage.ClickhouseV2 as CHV2
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Beckn.Ack
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.PersonExtra as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.Error

type AckResp = AckResponse

callBasedEndRide ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    HasField "enableAPILatencyLogging" r Bool,
    HasField "enableAPIPrometheusMetricLogging" r Bool,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    EncFlow m r,
    LT.HasLocationService m r,
    HasShortDurationRetryCfg r c,
    HasKafkaProducer r,
    CHV2.HasClickhouseEnv CHV2.APP_SERVICE_CLICKHOUSE m
  ) =>
  EndRide.ServiceHandle m ->
  Id Merchant ->
  DbHash ->
  Text ->
  m AckResp
callBasedEndRide shandle merchantId mobileNumberHash callFrom = do
  driver <- runInReplica $ QPerson.findByMobileNumberAndMerchantAndRole "+91" mobileNumberHash merchantId DP.DRIVER >>= fromMaybeM (PersonWithPhoneNotFound callFrom)
  activeRide <- runInReplica $ QRide.getActiveByDriverId driver.id >>= fromMaybeM (RideForDriverNotFound $ getId driver.id)
  void $ QRB.findById activeRide.bookingId >>= fromMaybeM (BookingNotFound $ getId activeRide.bookingId)
  void $ EndRide.callBasedEndRide shandle activeRide.id (EndRide.CallBasedEndRideReq driver)
  return Ack
