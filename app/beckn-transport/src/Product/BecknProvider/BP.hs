{-# LANGUAGE OverloadedLabels #-}

module Product.BecknProvider.BP where

import App.Types (FlowHandler)
import Beckn.Product.Validation.Context
  ( validateContextCommons,
    validateDomain,
  )
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import qualified Beckn.Types.Core.API.Cancel as API
import qualified Beckn.Types.Core.API.Status as API
import qualified Beckn.Types.Core.API.Update as API
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context (Context (..))
import Beckn.Types.Core.DecimalValue (convertAmountToDecimalValue)
import qualified Beckn.Types.Core.Domain as Domain
import Beckn.Types.Core.Order
  ( Order (..),
    OrderItem (OrderItem),
  )
import Beckn.Types.Core.Price
import Beckn.Types.Core.Scalar
import Beckn.Types.Id
import Beckn.Types.Mobility.Driver (Driver)
import qualified Beckn.Types.Mobility.Order as Mobility
import Beckn.Types.Mobility.Payload (Payload (..))
import Beckn.Types.Mobility.Route
import Beckn.Types.Mobility.Trip (Trip (..))
import qualified Beckn.Types.Mobility.Vehicle as BVehicle
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import Control.Lens ((?~))
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified ExternalAPI.Flow as ExternalAPI
import qualified ExternalAPI.Transform as ExternalAPITransform
import qualified Storage.Queries.DriverInformation as DriverInformation
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Organization as Organization
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.RideCancellationReason as QRCR
import qualified Storage.Queries.Quote as Quote
import qualified Storage.Queries.Ride as Ride
import qualified Storage.Queries.RideRequest as RideRequest
import qualified Storage.Queries.SearchRequest as SearchRequest
import qualified Storage.Queries.Vehicle as Vehicle
import qualified Test.RandomStrings as RS
import Types.Error
import Types.Metrics (CoreMetrics)
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.Person as Person
import qualified Types.Storage.RideCancellationReason as SRCR
import qualified Types.Storage.Ride as Ride
import qualified Types.Storage.RideRequest as SRideRequest
import qualified Types.Storage.SearchRequest as SearchRequest
import Utils.Common
import qualified Utils.Notifications as Notify
import qualified Types.Storage.Quote as SQuote

cancel ::
  Id Organization.Organization ->
  SignatureAuthResult Organization.Organization ->
  API.CancelReq ->
  FlowHandler AckResponse
cancel transporterId _ req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    let context = req.context
    validateContext "cancel" context
    let quoteId = req.message.order.id -- transporter search productInstId
    transporterOrg <-
      Organization.findOrganizationById transporterId
        >>= fromMaybeM OrgNotFound
    quote <- Quote.findById (Id quoteId) >>= fromMaybeM QuoteDoesNotExist
    ride <- Ride.findByQuoteId (quote.id) >>= fromMaybeM RideNotFound
    RideRequest.createFlow =<< mkRideReq (ride.id) (transporterOrg.shortId) SRideRequest.CANCELLATION
    return Ack

cancelRide ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    FCMFlow m r,
    CoreMetrics m
  ) =>
  Id Ride.Ride ->
  SRCR.RideCancellationReason ->
  m ()
cancelRide rideId rideCReason = do
  ride <- Ride.findById rideId >>= fromMaybeM RideDoesNotExist
  let quoteId = ride.quoteId
  quote <- Quote.findById quoteId >>= fromMaybeM QuoteNotFound
  cancelRideTransaction ride rideCReason
  logTagInfo ("rideId-" <> getId rideId) ("Cancellation reason " <> show rideCReason.source)
  fork "cancelRide - Notify BAP" $ do
    let transporterId = ride.organizationId
    transporter <-
      Organization.findOrganizationById transporterId
        >>= fromMaybeM OrgNotFound
    notifyCancelToGateway quote transporter rideCReason.source
    searchRequest <- SearchRequest.findById (ride.requestId) >>= fromMaybeM SearchRequestNotFound
    whenJust ride.personId $ \driverId -> do
      driver <- Person.findPersonById driverId >>= fromMaybeM PersonNotFound
      Notify.notifyOnCancel searchRequest driver.id driver.deviceToken rideCReason.source

cancelRideTransaction ::
  DBFlow m r =>
  Ride.Ride ->
  SRCR.RideCancellationReason ->
  m ()
cancelRideTransaction ride rideCReason = DB.runSqlDBTransaction $ do
  let mbPersonId = ride.personId
  whenJust mbPersonId updateDriverInfo
  Ride.updateStatus ride.id Ride.CANCELLED
  QRCR.create rideCReason
  where
    updateDriverInfo personId = do
      let driverId = cast personId
      DriverInformation.updateOnRide driverId False
      when (rideCReason.source == Mobility.ByDriver) $ QDriverStats.updateIdleTime driverId

notifyServiceStatusToGateway ::
  ( DBFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Organization.Organization ->
  SQuote.Quote ->
  Ride.Ride ->
  m ()
notifyServiceStatusToGateway transporter quote ride = do
  mkOnServiceStatusPayload (quote.id) ride
    >>= ExternalAPI.callBAP "on_status" API.onStatus transporter (quote.requestId) . Right

mkOnServiceStatusPayload :: MonadTime m => Id SQuote.Quote -> Ride.Ride -> m API.OnStatusReqMessage
mkOnServiceStatusPayload quoteId ride = do
  mkOrderRes quoteId (getId $ ride.productId) (show $ ride.status)
    <&> API.OnStatusReqMessage
  where
    mkOrderRes quoteId' productId status = do
      now <- getCurrentTime
      return $
        Order
          { id = getId quoteId',
            state = T.pack status,
            items = [OrderItem productId Nothing],
            created_at = now,
            updated_at = now,
            billing = Nothing,
            payment = Nothing,
            update_action = Nothing,
            quotation = Nothing
          }

notifyTripInfoToGateway ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Ride.Ride ->
  Id SearchRequest.SearchRequest ->
  Organization.Organization ->
  m ()
notifyTripInfoToGateway ride searchRequestId transporter = do
  mkOnUpdatePayload ride
    >>= ExternalAPI.callBAP "on_update" API.onUpdate transporter searchRequestId . Right

notifyCancelToGateway ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  SQuote.Quote ->
  Organization.Organization ->
  Mobility.CancellationSource ->
  m ()
notifyCancelToGateway quote transporter cancellationSource = do
  trip <- mkCancelTripObj quote
  order <- ExternalAPITransform.mkOrder quote.id (Just trip) $ Just cancellationSource
  ExternalAPI.callBAP "on_cancel" API.onCancel transporter (quote.requestId) . Right $ API.OnCancelReqMessage order

mkTrip :: (DBFlow m r, EncFlow m r) => Ride.Ride -> m Trip
mkTrip ride = do
  driver <- mapM mkDriverInfo $ ride.personId
  vehicle <- join <$> mapM mkVehicleInfo (ride.entityId)
  tripCode <- ride.udf4 & fromMaybeM (QuoteFieldNotPresent "udf4")
  logTagInfo "vehicle" $ show vehicle
  return $
    Trip
      { id = tripCode,
        pickup = Nothing,
        drop = Nothing,
        state = Nothing,
        vehicle = vehicle,
        driver,
        payload = Payload Nothing Nothing [] Nothing,
        fare = mkPrice <$> ride.actualPrice,
        route =
          Just $
            Route
              RouteEdge
                { path = "",
                  duration = emptyScalar "seconds", -- TODO: calculate duration and put it here
                  distance = emptyScalar "meters" & #computed_value ?~ ride.distance
                }
      }
  where
    mkPrice price =
      emptyPrice & #computed_value ?~ convertAmountToDecimalValue price

mkOnUpdatePayload ::
  (DBFlow m r, EncFlow m r) =>
  Ride.Ride ->
  m API.OnUpdateOrder
mkOnUpdatePayload ride = do
  trip <- mkTrip ride
  let quoteId = ride.quoteId
  order <- ExternalAPITransform.mkOrder quoteId (Just trip) Nothing
  return $ API.OnUpdateOrder order

mkDriverInfo :: (DBFlow m r, EncFlow m r) => Id Person.Person -> m Driver
mkDriverInfo driverId = do
  person <-
    Person.findPersonById driverId
      >>= fromMaybeM PersonNotFound
  ExternalAPITransform.mkDriverObj person

mkVehicleInfo :: DBFlow m r => Text -> m (Maybe BVehicle.Vehicle)
mkVehicleInfo vehicleId = do
  vehicle <- Vehicle.findVehicleById (Id vehicleId)
  return $ ExternalAPITransform.mkVehicleObj <$> vehicle

mkCancelTripObj :: (DBFlow m r, EncFlow m r) => SQuote.Quote -> m Trip
mkCancelTripObj quote = do
  driver <- mapM mkDriverInfo $ quote.personId
  vehicle <- join <$> mapM mkVehicleInfo (quote.entityId)
  return $
    Trip
      { id = getId $ quote.id,
        pickup = Nothing,
        drop = Nothing,
        state = Nothing,
        vehicle = vehicle,
        driver,
        payload = Payload Nothing Nothing [] Nothing,
        fare = Just $ ExternalAPITransform.mkPrice quote,
        route = Nothing
      }

getIdShortIdAndTime :: (MonadFlow m, GuidLike m b) => m (UTCTime, b, ShortId a)
getIdShortIdAndTime = do
  now <- getCurrentTime
  guid <- generateGUID
  shortId <- T.pack <$> L.runIO (RS.randomString (RS.onlyAlphaNum RS.randomASCII) 16)
  return (now, guid, ShortId shortId)

validateContext :: HasFlowEnv m r ["coreVersion" ::: Text, "domainVersion" ::: Text] => Text -> Context -> m ()
validateContext action context = do
  validateDomain Domain.MOBILITY context
  validateContextCommons action context

mkRideReq ::
  MonadFlow m =>
  Id Ride.Ride ->
  ShortId Organization.Organization ->
  SRideRequest.RideRequestType ->
  m SRideRequest.RideRequest
mkRideReq rideId shortOrgId rideRequestType = do
  guid <- generateGUID
  currTime <- getCurrentTime
  pure
    SRideRequest.RideRequest
      { id = Id guid,
        rideId = rideId,
        shortOrgId = shortOrgId,
        createdAt = currTime,
        _type = rideRequestType,
        info = Nothing
      }

notifyUpdateToBAP ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    FCMFlow m r,
    CoreMetrics m
  ) =>
  SQuote.Quote ->
  Ride.Ride ->
  Ride.RideStatus ->
  m ()
notifyUpdateToBAP quote ride updatedStatus = do
  -- Send callbacks to BAP
  transporter <-
    Organization.findOrganizationById quote.organizationId
      >>= fromMaybeM OrgNotFound
  notifyTripDetailsToGateway transporter quote ride
  notifyStatusUpdateReq transporter quote updatedStatus

notifyTripDetailsToGateway ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Organization.Organization ->
  SQuote.Quote ->
  Ride.Ride ->
  m ()
notifyTripDetailsToGateway transporter quote ride = do
  notifyTripInfoToGateway ride quote.requestId transporter

notifyStatusUpdateReq ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    FCMFlow m r,
    CoreMetrics m
  ) =>
  Organization.Organization ->
  SQuote.Quote ->
  Ride.RideStatus ->
  m ()
notifyStatusUpdateReq transporterOrg quote status = do
  case status of
    Ride.CANCELLED -> do
      admins <- getAdmins
      notifyCancelToGateway quote transporterOrg Mobility.ByOrganization
      Notify.notifyCancelReqByBP quote admins
    Ride.TRIP_REASSIGNMENT -> do
      admins <- getAdmins
      Notify.notifyDriverCancelledRideRequest quote admins
      notifyStatusToGateway
    _ -> notifyStatusToGateway
  where
    getAdmins = do
      if transporterOrg.enabled
        then Person.findAllByOrgId [Person.ADMIN] quote.organizationId
        else pure []
    notifyStatusToGateway = do
      ride <-
        Ride.findByQuoteId (quote.id)
          >>= fromMaybeM RideNotFound
      notifyServiceStatusToGateway transporterOrg quote ride
