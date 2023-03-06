{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module SharedLogic.CallBAP
  ( sendRideAssignedUpdateToBAP,
    sendRideStartedUpdateToBAP,
    sendRideCompletedUpdateToBAP,
    sendBookingCancelledUpdateToBAP,
    sendBookingReallocationUpdateToBAP,
    sendDriverArrivalUpdateToBAP,
    buildBppUrl,
  )
where

import qualified Beckn.ACL.OnUpdate as ACL
import qualified Beckn.Types.Core.Taxi.API.OnUpdate as API
import qualified Beckn.Types.Core.Taxi.OnUpdate as OnUpdate
import qualified Data.Text as T
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SRBCR
import qualified Domain.Types.FarePolicy.FareBreakup as DFareBreakup
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Ride as SRide
import EulerHS.Prelude
import Kernel.Storage.Hedis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes (BecknCallbackReq (BecknCallbackReq))
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError as Beckn
import Kernel.Utils.Servant.SignatureAuth
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Error
import Tools.Metrics (CoreMetrics)

sendRideAssignedUpdateToBAP ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    HedisFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  SRB.Booking ->
  SRide.Ride ->
  m ()
sendRideAssignedUpdateToBAP booking ride = do
  transporter <-
    CQM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (VehicleNotFound ride.driverId.getId)
  let rideAssignedBuildReq = ACL.RideAssignedBuildReq {..}
  rideAssignedMsg <- ACL.buildOnUpdateMessage rideAssignedBuildReq

  retryConfig <- asks (.shortDurationRetryCfg)

  void $ callOnUpdate transporter booking rideAssignedMsg retryConfig

sendRideStartedUpdateToBAP ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    HedisFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    CoreMetrics m
  ) =>
  SRB.Booking ->
  SRide.Ride ->
  m ()
sendRideStartedUpdateToBAP booking ride = do
  transporter <-
    CQM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  let rideStartedBuildReq = ACL.RideStartedBuildReq {..}
  rideStartedMsg <- ACL.buildOnUpdateMessage rideStartedBuildReq

  retryConfig <- asks (.longDurationRetryCfg)

  void $ callOnUpdate transporter booking rideStartedMsg retryConfig

sendRideCompletedUpdateToBAP ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    HedisFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    CoreMetrics m
  ) =>
  SRB.Booking ->
  SRide.Ride ->
  [DFareBreakup.FareBreakup] ->
  m ()
sendRideCompletedUpdateToBAP booking ride fareBreakups = do
  transporter <-
    CQM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  let rideCompletedBuildReq = ACL.RideCompletedBuildReq {..}
  rideCompletedMsg <- ACL.buildOnUpdateMessage rideCompletedBuildReq

  retryConfig <- asks (.longDurationRetryCfg)

  void $ callOnUpdate transporter booking rideCompletedMsg retryConfig

sendBookingCancelledUpdateToBAP ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    CoreMetrics m
  ) =>
  SRB.Booking ->
  DM.Merchant ->
  SRBCR.CancellationSource ->
  m ()
sendBookingCancelledUpdateToBAP booking transporter cancellationSource = do
  retryConfig <- asks (.longDurationRetryCfg)

  let bookingCancelledBuildReq = ACL.BookingCancelledBuildReq {..}
  bookingCancelledMsg <- ACL.buildOnUpdateMessage bookingCancelledBuildReq
  void $ callOnUpdate transporter booking bookingCancelledMsg retryConfig

sendBookingReallocationUpdateToBAP ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    CoreMetrics m
  ) =>
  SRB.Booking ->
  Id SRide.Ride ->
  DM.Merchant ->
  SRBCR.CancellationSource ->
  m ()
sendBookingReallocationUpdateToBAP booking rideId transporter reallocationSource = do
  let bookingReallocationBuildReq = ACL.BookingReallocationBuildReq {..}
  bookingReallocationMsg <- ACL.buildOnUpdateMessage bookingReallocationBuildReq

  retryConfig <- asks (.shortDurationRetryCfg)

  void $ callOnUpdate transporter booking bookingReallocationMsg retryConfig

callOnUpdate ::
  ( EsqDBFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasHttpClientOptions r c,
    CoreMetrics m
  ) =>
  DM.Merchant ->
  SRB.Booking ->
  OnUpdate.OnUpdateMessage ->
  RetryCfg ->
  m ()
callOnUpdate transporter booking content retryConfig = do
  let bapId = booking.bapId
      bapUri = booking.bapUri
  let bppSubscriberId = getShortId $ transporter.subscriberId
      authKey = getHttpManagerKey bppSubscriberId
  bppUri <- buildBppUrl transporter.id
  msgId <- generateGUID
  context <- buildTaxiContext Context.ON_UPDATE msgId (Just booking.transactionId) bapId bapUri (Just bppSubscriberId) (Just bppUri)
  void $ withRetryConfig retryConfig $ void . Beckn.callBecknAPI (Just authKey) Nothing (show Context.ON_UPDATE) API.onUpdateAPI bapUri . BecknCallbackReq context $ Right content

buildBppUrl ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Id DM.Merchant ->
  m BaseUrl
buildBppUrl (Id transporterId) =
  asks (.nwAddress)
    <&> #baseUrlPath %~ (<> "/" <> T.unpack transporterId)

sendDriverArrivalUpdateToBAP ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    EncFlow m r,
    HedisFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  SRB.Booking ->
  SRide.Ride ->
  Maybe UTCTime ->
  m ()
sendDriverArrivalUpdateToBAP booking ride arrivalTime = do
  transporter <-
    CQM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  let driverArrivedBuildReq = ACL.DriverArrivedBuildReq {ride, arrivalTime}
  driverArrivedMsg <- ACL.buildOnUpdateMessage driverArrivedBuildReq

  retryConfig <- asks (.shortDurationRetryCfg)

  void $ callOnUpdate transporter booking driverArrivedMsg retryConfig
