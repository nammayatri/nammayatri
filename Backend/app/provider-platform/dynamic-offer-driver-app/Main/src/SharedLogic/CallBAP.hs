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
    sendNewMessageToBAP,
    sendDriverOffer,
    callOnConfirm,
    buildBppUrl,
  )
where

import qualified AWS.S3 as S3
import qualified Beckn.ACL.OnSelect as ACL
import qualified Beckn.ACL.OnUpdate as ACL
import qualified Beckn.Types.Core.Taxi.API.OnConfirm as API
import qualified Beckn.Types.Core.Taxi.API.OnSelect as API
import qualified Beckn.Types.Core.Taxi.API.OnUpdate as API
import qualified Beckn.Types.Core.Taxi.OnConfirm as OnConfirm
import qualified Beckn.Types.Core.Taxi.OnSelect as OnSelect
import qualified Beckn.Types.Core.Taxi.OnUpdate as OnUpdate
import Control.Lens ((%~))
import Data.Either.Extra (eitherToMaybe)
import qualified Data.Text as T
import Domain.Action.UI.DriverOnboarding.AadhaarVerification
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as SRBCR
import qualified Domain.Types.DriverQuote as DDQ
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.FareParameters as Fare
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import qualified EulerHS.Types as ET
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError as Beckn
import Kernel.Utils.Servant.SignatureAuth
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.DriverInformation as QDI
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
  DST.SearchTry ->
  OnSelect.OnSelectMessage ->
  m ()
callOnSelect transporter searchRequest searchTry content = do
  let bapId = searchRequest.bapId
      bapUri = searchRequest.bapUri
  let bppSubscriberId = getShortId $ transporter.subscriberId
      authKey = getHttpManagerKey bppSubscriberId
  bppUri <- buildBppUrl (transporter.id)
  estimateId <- searchTry.estimateId & fromMaybeM (InternalError "SearchTry field not present: estimateId")
  let msgId = estimateId.getId
  context <- buildTaxiContext Context.ON_SELECT msgId (Just searchRequest.transactionId) bapId bapUri (Just bppSubscriberId) (Just bppUri) (fromMaybe transporter.city searchRequest.bapCity) (fromMaybe Context.India searchRequest.bapCountry) False
  logDebug $ "on_select request bpp: " <> show content
  void $ withShortRetry $ Beckn.callBecknAPI (Just $ ET.ManagerSelector authKey) Nothing (show Context.ON_SELECT) API.onSelectAPI bapUri . BecknCallbackReq context $ Right content

callOnUpdate ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m,
    HasHttpClientOptions r c
  ) =>
  DM.Merchant ->
  Text ->
  BaseUrl ->
  Maybe Context.City ->
  Maybe Context.Country ->
  Text ->
  OnUpdate.OnUpdateMessage ->
  RetryCfg ->
  m ()
callOnUpdate transporter bapId bapUri bapCity bapCountry transactionId content retryConfig = do
  let bppSubscriberId = getShortId $ transporter.subscriberId
      authKey = getHttpManagerKey bppSubscriberId
  bppUri <- buildBppUrl (transporter.id)
  msgId <- generateGUID
  context <- buildTaxiContext Context.ON_UPDATE msgId (Just transactionId) bapId bapUri (Just bppSubscriberId) (Just bppUri) (fromMaybe transporter.city bapCity) (fromMaybe Context.India bapCountry) False
  void $ withRetryConfig retryConfig $ Beckn.callBecknAPI (Just $ ET.ManagerSelector authKey) Nothing (show Context.ON_UPDATE) API.onUpdateAPI bapUri . BecknCallbackReq context $ Right content

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
      city = contextFromConfirm.city
      country = contextFromConfirm.country
      bppSubscriberId = getShortId $ transporter.subscriberId
      authKey = getHttpManagerKey bppSubscriberId
  bppUri <- buildBppUrl transporter.id
  context_ <- buildTaxiContext Context.ON_CONFIRM msgId contextFromConfirm.transaction_id bapId bapUri (Just bppSubscriberId) (Just bppUri) city country False
  void $ withShortRetry $ Beckn.callBecknAPI (Just $ ET.ManagerSelector authKey) Nothing (show Context.ON_CONFIRM) API.onConfirmAPI bapUri . BecknCallbackReq context_ $ Right content

buildBppUrl ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  Id DM.Merchant ->
  m BaseUrl
buildBppUrl (Id transporterId) =
  asks (.nwAddress)
    <&> #baseUrlPath %~ (<> "/" <> T.unpack transporterId)

sendRideAssignedUpdateToBAP ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    CacheFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasField "s3Env" r (S3.S3Env m),
    MonadReader r m,
    MonadFlow m,
    MonadTime m,
    CacheFlow m r
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
  driverInfo <- QDI.findById (cast ride.driverId) >>= fromMaybeM DriverInfoNotFound
  resp <- try @_ @SomeException (fetchAndCacheAadhaarImage driver driverInfo)
  let image = join (eitherToMaybe resp)
  let rideAssignedBuildReq = ACL.RideAssignedBuildReq {..}
  rideAssignedMsg <- ACL.buildOnUpdateMessage rideAssignedBuildReq

  retryConfig <- asks (.shortDurationRetryCfg)

  void $ callOnUpdate transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId rideAssignedMsg retryConfig

sendRideStartedUpdateToBAP ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  m ()
sendRideStartedUpdateToBAP booking ride = do
  transporter <-
    CQM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (VehicleNotFound ride.driverId.getId)
  let rideStartedBuildReq = ACL.RideStartedBuildReq {..}
  rideStartedMsg <- ACL.buildOnUpdateMessage rideStartedBuildReq

  retryConfig <- asks (.longDurationRetryCfg)

  void $ callOnUpdate transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId rideStartedMsg retryConfig

sendRideCompletedUpdateToBAP ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  Fare.FareParameters ->
  Maybe DMPM.PaymentMethodInfo ->
  Maybe Text ->
  m ()
sendRideCompletedUpdateToBAP booking ride fareParams paymentMethodInfo paymentUrl = do
  transporter <-
    CQM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (VehicleNotFound ride.driverId.getId)
  let rideCompletedBuildReq = ACL.RideCompletedBuildReq {..}
  rideCompletedMsg <- ACL.buildOnUpdateMessage rideCompletedBuildReq

  retryConfig <- asks (.longDurationRetryCfg)

  void $ callOnUpdate transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId rideCompletedMsg retryConfig

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

  void $ callOnUpdate transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId bookingCancelledMsg retryConfig

sendDriverOffer ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    CoreMetrics m,
    HasPrettyLogger m r
  ) =>
  DM.Merchant ->
  DSR.SearchRequest ->
  DST.SearchTry ->
  DDQ.DriverQuote ->
  m ()
sendDriverOffer transporter searchReq searchTry driverQuote = do
  logDebug $ "on_select ttl request driver: " <> show driverQuote.validTill
  callOnSelect transporter searchReq searchTry =<< (buildOnSelectReq transporter searchReq driverQuote <&> ACL.mkOnSelectMessage)
  where
    buildOnSelectReq ::
      (MonadTime m, HasPrettyLogger m r) =>
      DM.Merchant ->
      DSR.SearchRequest ->
      DDQ.DriverQuote ->
      m ACL.DOnSelectReq
    buildOnSelectReq org searchRequest quotes = do
      now <- getCurrentTime
      --logPretty DEBUG "on_select: searchRequest" searchRequest
      logPretty DEBUG "on_select: quotes" quotes
      let transporterInfo =
            ACL.TransporterInfo
              { merchantShortId = org.shortId,
                name = org.name,
                contacts = fromMaybe "" org.mobileNumber,
                ridesInProgress = 0, -- FIXME
                ridesCompleted = 0, -- FIXME
                ridesConfirmed = 0 -- FIXME
              }
      pure $
        ACL.DOnSelectReq
          { transporterInfo,
            driverQuote,
            now,
            searchRequest
          }

sendDriverArrivalUpdateToBAP ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  Maybe UTCTime ->
  m ()
sendDriverArrivalUpdateToBAP booking ride arrivalTime = do
  transporter <-
    CQM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (VehicleNotFound ride.driverId.getId)
  let driverArrivedBuildReq = ACL.DriverArrivedBuildReq {..}
  driverArrivedMsg <- ACL.buildOnUpdateMessage driverArrivedBuildReq

  retryConfig <- asks (.shortDurationRetryCfg)

  void $ callOnUpdate transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId driverArrivedMsg retryConfig

sendNewMessageToBAP ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  T.Text ->
  m ()
sendNewMessageToBAP booking ride message = do
  transporter <-
    CQM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (VehicleNotFound ride.driverId.getId)
  let newMessageBuildReq = ACL.NewMessageBuildReq {..}
  newMessageMsg <- ACL.buildOnUpdateMessage newMessageBuildReq
  retryConfig <- asks (.shortDurationRetryCfg)
  void $ callOnUpdate transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId newMessageMsg retryConfig

sendEstimateRepetitionUpdateToBAP ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  Id DEst.Estimate ->
  SRBCR.CancellationSource ->
  m ()
sendEstimateRepetitionUpdateToBAP booking ride estimateId cancellationSource = do
  transporter <-
    CQM.findById booking.providerId
      >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  let estimateRepetitionBuildReq = ACL.EstimateRepetitionBuildReq {cancellationSource, booking, estimateId, ride}
  estimateRepMsg <- ACL.buildOnUpdateMessage estimateRepetitionBuildReq
  retryConfig <- asks (.shortDurationRetryCfg)
  void $ callOnUpdate transporter booking.bapId booking.bapUri booking.bapCity booking.bapCountry booking.transactionId estimateRepMsg retryConfig
