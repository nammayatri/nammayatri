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
import EulerHS.Prelude
import qualified ExternalAPI.Flow as ExternalAPI
import qualified ExternalAPI.Transform as ExternalAPITransform
import qualified Storage.Queries.DriverInformation as DriverInformation
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Organization as Organization
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Quote as Quote
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import qualified Storage.Queries.RideRequest as RideRequest
import qualified Storage.Queries.SearchRequest as SearchRequest
import qualified Storage.Queries.Vehicle as Vehicle
import Types.Error
import Types.Metrics (CoreMetrics)
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.Person as Person
import qualified Types.Storage.Quote as SQ
import qualified Types.Storage.Quote as SQuote
import qualified Types.Storage.Ride as SRide
import qualified Types.Storage.RideBooking as SRB
import qualified Types.Storage.RideCancellationReason as SRCR
import qualified Types.Storage.RideRequest as SRideRequest
import qualified Types.Storage.Vehicle as SVeh
import Utils.Common
import qualified Utils.Notifications as Notify

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
    rideBooking <- QRB.findByQuoteId (quote.id) >>= fromMaybeM RideNotFound
    now <- getCurrentTime
    RideRequest.createFlow =<< mkRideReq (rideBooking.id) (transporterOrg.shortId) SRideRequest.CANCELLATION now
    return Ack

cancelRide ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    FCMFlow m r,
    CoreMetrics m
  ) =>
  Id SRide.Ride ->
  SRCR.RideCancellationReason ->
  m ()
cancelRide rideId rideCReason = do
  ride <- QRide.findById rideId >>= fromMaybeM RideDoesNotExist
  rideBooking <- QRB.findById ride.bookingId >>= fromMaybeM RideBookingNotFound
  cancelRideTransaction rideBooking ride rideCReason
  logTagInfo ("rideId-" <> getId rideId) ("Cancellation reason " <> show rideCReason.source)
  fork "cancelRide - Notify BAP" $ do
    let transporterId = rideBooking.providerId
    transporter <-
      Organization.findOrganizationById transporterId
        >>= fromMaybeM OrgNotFound
    notifyCancelToGateway rideBooking (Just ride) transporter rideCReason.source
    searchRequest <- SearchRequest.findById (rideBooking.requestId) >>= fromMaybeM SearchRequestNotFound
    driver <- Person.findPersonById ride.driverId >>= fromMaybeM PersonNotFound
    Notify.notifyOnCancel searchRequest driver.id driver.deviceToken rideCReason.source

cancelRideTransaction ::
  DBFlow m r =>
  SRB.RideBooking ->
  SRide.Ride ->
  SRCR.RideCancellationReason ->
  m ()
cancelRideTransaction rideBooking ride rideCReason = DB.runSqlDBTransaction $ do
  updateDriverInfo ride.driverId
  QRide.updateStatus ride.id SRide.CANCELLED
  QRB.updateStatus rideBooking.id SRB.CANCELLED
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
  SQ.Quote ->
  SRB.RideBooking ->
  m ()
notifyServiceStatusToGateway transporter quote rideBooking = do
  mkOnServiceStatusPayload quote rideBooking
    >>= ExternalAPI.callBAP "on_status" API.onStatus transporter (rideBooking.requestId) . Right

mkOnServiceStatusPayload :: MonadTime m => SQ.Quote -> SRB.RideBooking -> m API.OnStatusReqMessage
mkOnServiceStatusPayload quote rideBooking = do
  mkOrderRes quote.id (getId $ quote.productId) (show $ rideBooking.status)
    <&> API.OnStatusReqMessage
  where
    mkOrderRes quoteId productId status = do
      now <- getCurrentTime
      return $
        Order
          { id = getId quoteId,
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
  SRB.RideBooking ->
  SRide.Ride ->
  Organization.Organization ->
  Mobility.OrderState ->
  m ()
notifyTripInfoToGateway rideBooking ride transporter orderState = do
  mkOnUpdatePayload rideBooking ride orderState
    >>= ExternalAPI.callBAP "on_update" API.onUpdate transporter rideBooking.requestId . Right

notifyCancelToGateway ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  SRB.RideBooking ->
  Maybe SRide.Ride ->
  Organization.Organization ->
  Mobility.CancellationSource ->
  m ()
notifyCancelToGateway rideBooking mbRide transporter cancellationSource = do
  trip <- mkCancelTripObj rideBooking `traverse` mbRide
  order <- ExternalAPITransform.mkOrder rideBooking.quoteId rideBooking.id trip (Just cancellationSource) Mobility.CANCELLED
  ExternalAPI.callBAP "on_cancel" API.onCancel transporter rideBooking.requestId . Right $ API.OnCancelReqMessage order

mkTrip :: (DBFlow m r, EncFlow m r) => SRide.Ride -> m Trip
mkTrip ride = do
  driver <- mkDriverInfo ride.driverId
  vehicle <- mkVehicleInfo ride.vehicleId
  logTagInfo "vehicle" $ show vehicle
  return $
    Trip
      { id = ride.otp,
        pickup = Nothing,
        drop = Nothing,
        state = Nothing,
        vehicle = vehicle,
        driver = Just driver,
        payload = Payload Nothing Nothing [] Nothing,
        fare = mkPrice <$> ride.finalPrice,
        totalFare = mkPrice <$> ride.totalFare,
        route =
          Just $
            Route
              RouteEdge
                { path = "",
                  duration = emptyScalar "seconds", -- TODO: calculate duration and put it here
                  distance = emptyScalar "meters" & #value .~ ride.chargeableDistance
                }
      }
  where
    mkPrice price =
      emptyPrice & #value ?~ convertAmountToDecimalValue price

mkOnUpdatePayload ::
  (DBFlow m r, EncFlow m r) =>
  SRB.RideBooking ->
  SRide.Ride ->
  Mobility.OrderState ->
  m API.OnUpdateOrder
mkOnUpdatePayload rideBooking ride orderState = do
  trip <- mkTrip ride
  order <- ExternalAPITransform.mkOrder rideBooking.quoteId rideBooking.id (Just trip) Nothing orderState
  return $ API.OnUpdateOrder order

mkDriverInfo :: (DBFlow m r, EncFlow m r) => Id Person.Person -> m Driver
mkDriverInfo driverId = do
  person <-
    Person.findPersonById driverId
      >>= fromMaybeM PersonNotFound
  ExternalAPITransform.mkDriverObj person

mkVehicleInfo :: DBFlow m r => Id SVeh.Vehicle -> m (Maybe BVehicle.Vehicle)
mkVehicleInfo vehicleId = do
  vehicle <- Vehicle.findVehicleById vehicleId
  return $ ExternalAPITransform.mkVehicleObj <$> vehicle

mkCancelTripObj :: (DBFlow m r, EncFlow m r) => SRB.RideBooking -> SRide.Ride -> m Trip
mkCancelTripObj rideBooking ride = do
  driver <- mkDriverInfo ride.driverId
  vehicle <- mkVehicleInfo ride.vehicleId
  return $
    Trip
      { id = getId ride.id,
        pickup = Nothing,
        drop = Nothing,
        state = Nothing,
        vehicle = vehicle,
        driver = Just driver,
        payload = Payload Nothing Nothing [] Nothing,
        fare = Just $ ExternalAPITransform.mkPrice rideBooking.price,
        totalFare = Just $ ExternalAPITransform.mkPrice rideBooking.estimatedTotalFare,
        route = Nothing
      }

validateContext :: HasFlowEnv m r ["coreVersion" ::: Text, "domainVersion" ::: Text] => Text -> Context -> m ()
validateContext action context = do
  validateDomain Domain.MOBILITY context
  validateContextCommons action context

mkRideReq ::
  MonadFlow m =>
  Id SRB.RideBooking ->
  ShortId Organization.Organization ->
  SRideRequest.RideRequestType ->
  UTCTime ->
  m SRideRequest.RideRequest
mkRideReq rideId shortOrgId rideRequestType now = do
  guid <- generateGUID
  pure
    SRideRequest.RideRequest
      { id = Id guid,
        rideBookingId = rideId,
        shortOrgId = shortOrgId,
        createdAt = now,
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
  SRB.RideBooking ->
  SRide.Ride ->
  Mobility.OrderState ->
  m ()
notifyUpdateToBAP quote rideBooking ride orderState = do
  -- Send callbacks to BAP
  transporter <-
    Organization.findOrganizationById rideBooking.providerId
      >>= fromMaybeM OrgNotFound
  notifyTripDetailsToGateway transporter rideBooking ride orderState
  notifyStatusUpdateReq transporter quote rideBooking ride orderState

notifyTripDetailsToGateway ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Organization.Organization ->
  SRB.RideBooking ->
  SRide.Ride ->
  Mobility.OrderState ->
  m ()
notifyTripDetailsToGateway transporter rideBooking ride orderState = do
  notifyTripInfoToGateway rideBooking ride transporter orderState

notifyStatusUpdateReq ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    FCMFlow m r,
    CoreMetrics m
  ) =>
  Organization.Organization ->
  SQuote.Quote ->
  SRB.RideBooking ->
  SRide.Ride ->
  Mobility.OrderState ->
  m ()
notifyStatusUpdateReq transporterOrg quote rideBooking ride status = do
  case status of
    Mobility.CANCELLED -> do
      admins <- getAdmins
      notifyCancelToGateway rideBooking (Just ride) transporterOrg Mobility.ByOrganization
      Notify.notifyCancelReqByBP rideBooking admins
    _ -> notifyStatusToGateway
  where
    getAdmins = do
      if transporterOrg.enabled
        then Person.findAllByOrgId [Person.ADMIN] rideBooking.providerId
        else pure []
    notifyStatusToGateway = do
      notifyServiceStatusToGateway transporterOrg quote rideBooking
