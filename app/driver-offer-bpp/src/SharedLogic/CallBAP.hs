{-# LANGUAGE OverloadedLabels #-}

module SharedLogic.CallBAP
  ( sendRideAssignedUpdateToBAP,
    sendRideStartedUpdateToBAP,
    sendRideCompletedUpdateToBAP,
    sendBookingCancelledUpdateToBAP,
    sendDriverOffer,
    callOnConfirm,
    buildBppUrl,
  )
where

import Beckn.Types.Common
import qualified Beckn.Types.Core.Context as Context
import Beckn.Types.Core.ReqTypes
import qualified Beckn.Types.Core.Taxi.API.OnConfirm as API
import qualified Beckn.Types.Core.Taxi.API.OnSelect as API
import qualified Beckn.Types.Core.Taxi.API.OnUpdate as API
import qualified Beckn.Types.Core.Taxi.OnConfirm as OnConfirm
import qualified Beckn.Types.Core.Taxi.OnSelect as OnSelect
import qualified Beckn.Types.Core.Taxi.OnUpdate as OnUpdate
import Beckn.Types.Id
import qualified Beckn.Utils.Error.BaseError.HTTPError.BecknAPIError as Beckn
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.ACL.OnSelect as ACL
import qualified Core.ACL.OnUpdate as ACL
import qualified Data.Text as T
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as SRBCR
import qualified Domain.Types.DriverQuote as DDQ
import qualified Domain.Types.FareParams as Fare
import Domain.Types.Organization as Org
import qualified Domain.Types.Organization as SOrg
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.SearchRequest as DSR
import EulerHS.Prelude
import qualified Storage.Queries.Organization as QOrg
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Metrics (CoreMetrics)
import Types.Error
import Utils.Common

callOnSelect ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Org.Organization ->
  DSR.SearchRequest ->
  OnSelect.OnSelectMessage ->
  m ()
callOnSelect transporter searchRequest content = do
  let bapId = searchRequest.bapId
      bapUri = searchRequest.bapUri
  let bppShortId = getShortId $ transporter.shortId
      authKey = getHttpManagerKey bppShortId
  bppUri <- buildBppUrl (transporter.id)
  let msgId = searchRequest.messageId
  context <- buildTaxiContext Context.ON_SELECT msgId Nothing bapId bapUri (Just transporter.shortId.getShortId) (Just bppUri)
  logDebug $ "on_select request bpp: " <> show content
  void . Beckn.callBecknAPI (Just authKey) Nothing (show Context.ON_SELECT) API.onSelectAPI bapUri . BecknCallbackReq context $ Right content

callOnUpdate ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Org.Organization ->
  DRB.Booking ->
  OnUpdate.OnUpdateMessage ->
  m ()
callOnUpdate transporter booking content = do
  let bapId = booking.bapId
      bapUri = booking.bapUri
  let bppShortId = getShortId $ transporter.shortId
      authKey = getHttpManagerKey bppShortId
  bppUri <- buildBppUrl (transporter.id)
  msgId <- generateGUID
  context <- buildTaxiContext Context.ON_UPDATE msgId Nothing bapId bapUri (Just transporter.shortId.getShortId) (Just bppUri)
  void . Beckn.callBecknAPI (Just authKey) Nothing (show Context.ON_UPDATE) API.onUpdateAPI bapUri . BecknCallbackReq context $ Right content

callOnConfirm ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Org.Organization ->
  Context.Context ->
  OnConfirm.OnConfirmMessage ->
  m ()
callOnConfirm transporter contextFromConfirm content = do
  let bapUri = contextFromConfirm.bap_uri
      bapId = contextFromConfirm.bap_id
      msgId = contextFromConfirm.message_id
      bppShortId = getShortId $ transporter.shortId
      authKey = getHttpManagerKey bppShortId
  bppUri <- buildBppUrl transporter.id
  context_ <- buildTaxiContext Context.ON_CONFIRM msgId Nothing bapId bapUri (Just transporter.shortId.getShortId) (Just bppUri)
  void $ Beckn.callBecknAPI (Just authKey) Nothing (show Context.ON_CONFIRM) API.onConfirmAPI bapUri . BecknCallbackReq context_ $ Right content

buildBppUrl ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  Id Org.Organization ->
  m BaseUrl
buildBppUrl (Id transporterId) =
  asks (.nwAddress)
    <&> #baseUrlPath %~ (<> "/" <> T.unpack transporterId)

sendRideAssignedUpdateToBAP ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  m ()
sendRideAssignedUpdateToBAP booking ride = do
  transporter <-
    QOrg.findById booking.providerId
      >>= fromMaybeM (OrgNotFound booking.providerId.getId)
  driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (VehicleNotFound ride.driverId.getId)
  let rideAssignedBuildReq = ACL.RideAssignedBuildReq {..}
  rideAssignedMsg <- ACL.buildOnUpdateMessage rideAssignedBuildReq
  void $ callOnUpdate transporter booking rideAssignedMsg

sendRideStartedUpdateToBAP ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  m ()
sendRideStartedUpdateToBAP booking ride = do
  transporter <-
    QOrg.findById booking.providerId
      >>= fromMaybeM (OrgNotFound booking.providerId.getId)
  let rideStartedBuildReq = ACL.RideStartedBuildReq {..}
  rideStartedMsg <- ACL.buildOnUpdateMessage rideStartedBuildReq
  void $ callOnUpdate transporter booking rideStartedMsg

sendRideCompletedUpdateToBAP ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  DRB.Booking ->
  SRide.Ride ->
  Fare.FareParameters ->
  Money ->
  m ()
sendRideCompletedUpdateToBAP booking ride fareParams finalFare = do
  transporter <-
    QOrg.findById booking.providerId
      >>= fromMaybeM (OrgNotFound booking.providerId.getId)
  let rideCompletedBuildReq = ACL.RideCompletedBuildReq {ride, fareParams, finalFare}
  rideCompletedMsg <- ACL.buildOnUpdateMessage rideCompletedBuildReq
  void $ callOnUpdate transporter booking rideCompletedMsg

sendBookingCancelledUpdateToBAP ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  DRB.Booking ->
  SOrg.Organization ->
  SRBCR.CancellationSource ->
  m ()
sendBookingCancelledUpdateToBAP booking transporter cancellationSource = do
  let bookingCancelledBuildReq = ACL.BookingCancelledBuildReq {..}
  bookingCancelledMsg <- ACL.buildOnUpdateMessage bookingCancelledBuildReq
  void $ callOnUpdate transporter booking bookingCancelledMsg

sendDriverOffer ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m,
    HasPrettyLogger m r
  ) =>
  SOrg.Organization ->
  DSR.SearchRequest ->
  DDQ.DriverQuote ->
  m ()
sendDriverOffer transporter searchReq driverQuote = do
  callOnSelect transporter searchReq =<< (buildOnSelectReq transporter searchReq [driverQuote] <&> ACL.mkOnSelectMessage)
  where
    buildOnSelectReq ::
      (MonadTime m, HasPrettyLogger m r) =>
      SOrg.Organization ->
      DSR.SearchRequest ->
      [DDQ.DriverQuote] ->
      m ACL.DOnSelectReq
    buildOnSelectReq org searchRequest quotes = do
      now <- getCurrentTime
      logPretty DEBUG "on_select: searchRequest" searchRequest
      logPretty DEBUG "on_select: quotes" quotes
      let transporterInfo =
            ACL.TransporterInfo
              { shortId = org.shortId,
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
