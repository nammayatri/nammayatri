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
    sendDriverArrivalUpdateToBAP,
    sendEstimateRepetitionUpdateToBAP,
    sendDriverOffer,
    callOnConfirm,
    buildBppUrl,
  )
where

import qualified Beckn.ACL.OnSelect as ACL
import qualified Beckn.ACL.OnUpdate as ACL
import qualified Beckn.Types.Core.Taxi.API.OnConfirm as API
import qualified Beckn.Types.Core.Taxi.API.OnSelect as API
import qualified Beckn.Types.Core.Taxi.API.OnUpdate as API
import qualified Beckn.Types.Core.Taxi.OnConfirm as OnConfirm
import qualified Beckn.Types.Core.Taxi.OnSelect as OnSelect
import qualified Beckn.Types.Core.Taxi.OnUpdate as OnUpdate
import Control.Lens ((%~))
import qualified Data.Text as T
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as SRBCR
import qualified Domain.Types.DriverQuote as DDQ
import qualified Domain.Types.FareParameters as Fare
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.SearchRequest as DSR
import Kernel.Prelude
import Kernel.Storage.Hedis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError as Beckn
import Kernel.Utils.Servant.SignatureAuth
import qualified SharedLogic.Estimate as DEstimate
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Error
import Tools.Metrics (CoreMetrics)

callOnSelect ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c
  ) =>
  DM.Merchant ->
  DSR.SearchRequest ->
  OnSelect.OnSelectMessage ->
  m ()
callOnSelect transporter searchRequest content = do
  let bapId = searchRequest.bapId
      bapUri = searchRequest.bapUri
  let bppSubscriberId = getShortId $ transporter.subscriberId
      authKey = getHttpManagerKey bppSubscriberId
  bppUri <- buildBppUrl (transporter.id)
  let msgId = searchRequest.messageId
  context <- buildTaxiContext Context.ON_SELECT msgId (Just searchRequest.transactionId) bapId bapUri (Just bppSubscriberId) (Just bppUri)
  logDebug $ "on_select request bpp: " <> show content
  void $ withShortRetry $ Beckn.callBecknAPI (Just authKey) Nothing (show Context.ON_SELECT) API.onSelectAPI bapUri . BecknCallbackReq context $ Right content

callOnUpdate ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m,
    HasHttpClientOptions r c
  ) =>
  DM.Merchant ->
  Text ->
  BaseUrl ->
  Text ->
  OnUpdate.OnUpdateMessage ->
  RetryCfg ->
  m ()
callOnUpdate transporter bapId bapUri transactionId content retryConfig = do
  let bppSubscriberId = getShortId $ transporter.subscriberId
      authKey = getHttpManagerKey bppSubscriberId
  bppUri <- buildBppUrl (transporter.id)
  msgId <- generateGUID
  context <- buildTaxiContext Context.ON_UPDATE msgId (Just transactionId) bapId bapUri (Just bppSubscriberId) (Just bppUri)
  void $ withRetryConfig retryConfig $ Beckn.callBecknAPI (Just authKey) Nothing (show Context.ON_UPDATE) API.onUpdateAPI bapUri . BecknCallbackReq context $ Right content

callOnConfirm ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    CoreMetrics m
  ) =>
  DM.Merchant ->
  Context.Context ->
  OnConfirm.OnConfirmMessage ->
  m ()
callOnConfirm transporter contextFromConfirm content = do
  let bapUri = contextFromConfirm.bap_uri
      bapId = contextFromConfirm.bap_id
      msgId = contextFromConfirm.message_id
      bppSubscriberId = getShortId $ transporter.subscriberId
      authKey = getHttpManagerKey bppSubscriberId
  bppUri <- buildBppUrl transporter.id
  context_ <- buildTaxiContext Context.ON_CONFIRM msgId contextFromConfirm.transaction_id bapId bapUri (Just bppSubscriberId) (Just bppUri)
  void $ withShortRetry $ Beckn.callBecknAPI (Just authKey) Nothing (show Context.ON_CONFIRM) API.onConfirmAPI bapUri . BecknCallbackReq context_ $ Right content

buildBppUrl ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  Id DM.Merchant ->
  m BaseUrl
buildBppUrl (Id transporterId) =
  asks (.nwAddress)
    <&> #baseUrlPath %~ (<> "/" <> T.unpack transporterId)

sendRideAssignedUpdateToBAP ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HedisFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  DRB.Booking ->
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

  void $ callOnUpdate transporter booking.bapId booking.bapUri booking.transactionId rideAssignedMsg retryConfig

sendRideStartedUpdateToBAP ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HedisFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  m ()
sendRideStartedUpdateToBAP booking ride = do
  transporter <-
    CQM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  let rideStartedBuildReq = ACL.RideStartedBuildReq {..}
  rideStartedMsg <- ACL.buildOnUpdateMessage rideStartedBuildReq

  retryConfig <- asks (.longDurationRetryCfg)

  void $ callOnUpdate transporter booking.bapId booking.bapUri booking.transactionId rideStartedMsg retryConfig

sendRideCompletedUpdateToBAP ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    EncFlow m r,
    HedisFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  Fare.FareParameters ->
  m ()
sendRideCompletedUpdateToBAP booking ride fareParams = do
  transporter <-
    CQM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  let rideCompletedBuildReq = ACL.RideCompletedBuildReq {ride, fareParams}
  rideCompletedMsg <- ACL.buildOnUpdateMessage rideCompletedBuildReq

  retryConfig <- asks (.longDurationRetryCfg)

  void $ callOnUpdate transporter booking.bapId booking.bapUri booking.transactionId rideCompletedMsg retryConfig

sendBookingCancelledUpdateToBAP ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    CoreMetrics m
  ) =>
  DRB.Booking ->
  DM.Merchant ->
  SRBCR.CancellationSource ->
  m ()
sendBookingCancelledUpdateToBAP booking transporter cancellationSource = do
  let bookingCancelledBuildReq = ACL.BookingCancelledBuildReq {..}
  bookingCancelledMsg <- ACL.buildOnUpdateMessage bookingCancelledBuildReq

  retryConfig <- asks (.longDurationRetryCfg)

  void $ callOnUpdate transporter booking.bapId booking.bapUri booking.transactionId bookingCancelledMsg retryConfig

sendDriverOffer ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    CoreMetrics m,
    HasPrettyLogger m r
  ) =>
  DM.Merchant ->
  DSR.SearchRequest ->
  DDQ.DriverQuote ->
  m ()
sendDriverOffer transporter searchReq driverQuote = do
  callOnSelect transporter searchReq =<< (buildOnSelectReq transporter searchReq [driverQuote] <&> ACL.mkOnSelectMessage)
  where
    buildOnSelectReq ::
      (MonadTime m, HasPrettyLogger m r) =>
      DM.Merchant ->
      DSR.SearchRequest ->
      [DDQ.DriverQuote] ->
      m ACL.DOnSelectReq
    buildOnSelectReq org searchRequest quotes = do
      now <- getCurrentTime
      logPretty DEBUG "on_select: searchRequest" searchRequest
      logPretty DEBUG "on_select: quotes" quotes
      let transporterInfo =
            ACL.TransporterInfo
              { subscriberId = org.subscriberId,
                name = org.name,
                contacts = fromMaybe "" org.mobileNumber,
                ridesInProgress = 0, -- FIXME
                ridesCompleted = 0, -- FIXME
                ridesConfirmed = 0 -- FIXME
              }
      pure $
        ACL.DOnSelectReq
          { transporterInfo,
            quotes,
            now,
            searchRequest
          }

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
  DRB.Booking ->
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

  void $ callOnUpdate transporter booking.bapId booking.bapUri booking.transactionId driverArrivedMsg retryConfig

sendEstimateRepetitionUpdateToBAP ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    EncFlow m r,
    HedisFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  DEstimate.EstimateItem ->
  SRBCR.CancellationSource ->
  m ()
sendEstimateRepetitionUpdateToBAP booking ride estimateItem cancellationSource = do
  transporter <-
    CQM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  let estimateRepetitionBuildReq = ACL.EstimateRepetitionBuildReq {cancellationSource, booking, estimateItem, ride}
  estimateRepMsg <- ACL.buildOnUpdateMessage estimateRepetitionBuildReq
  retryConfig <- asks (.shortDurationRetryCfg)
  void $ callOnUpdate transporter booking.bapId booking.bapUri booking.transactionId estimateRepMsg retryConfig
