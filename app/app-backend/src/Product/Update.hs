module Product.Update where

import App.Types
import Beckn.Product.Validation.Context (validateContext)
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Taxi.API.OnUpdate as OnUpdate
import qualified Beckn.Types.Core.Taxi.OnUpdate as OnUpdate
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import EulerHS.Prelude hiding (state)
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import qualified Storage.Queries.RideCancellationReason as QRCR
import Types.Error
import Types.Metrics (CoreMetrics)
import qualified Types.Storage.Ride as SRide
import qualified Types.Storage.RideBooking as SRB
import qualified Types.Storage.RideCancellationReason as SRCR
import Utils.Common
import qualified Utils.Notifications as Notify

onUpdate ::
  SignatureAuthResult ->
  OnUpdate.OnUpdateReq ->
  FlowHandler AckResponse
onUpdate _org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    -- TODO: Verify api key here
    logTagInfo "on_update req" (show req)
    validateContext req.context
    processEvent req.message.cabs_update_event
    return Ack

processEvent :: (DBFlow m r, FCMFlow m r, CoreMetrics m) => OnUpdate.OnUpdateEvent -> m ()
processEvent (OnUpdate.RideAssigned taEvent) = do
  let bppBookingId = Id taEvent.order_id
  rideBooking <- QRB.findByBPPBookingId bppBookingId >>= fromMaybeM RideBookingDoesNotExist
  unless (rideBooking.status == SRB.CONFIRMED) $ throwError (RideBookingInvalidStatus $ show rideBooking.status)
  ride <- buildRide rideBooking
  DB.runSqlDBTransaction $ do
    QRB.updateStatus rideBooking.id SRB.TRIP_ASSIGNED
    QRide.create ride
  Notify.notifyOnRideAssigned rideBooking ride
  where
    buildRide :: MonadFlow m => SRB.RideBooking -> m SRide.Ride
    buildRide rideBooking = do
      guid <- generateGUID
      shortId <- generateShortId
      now <- getCurrentTime
      let bppRideId = Id taEvent.ride_id
          otp = taEvent.otp
          driverName = taEvent.agent.name
          driverMobileNumber = taEvent.agent.phone
          driverRating = realToFrac <$> taEvent.agent.rating
          driverRegisteredAt = taEvent.agent.registered_at
          vehicleNumber = taEvent.vehicle.registration
          vehicleColor = taEvent.vehicle.color
          vehicleModel = taEvent.vehicle.model
      return
        SRide.Ride
          { id = guid,
            bookingId = rideBooking.id,
            status = SRide.NEW,
            trackingUrl = "UNKNOWN", -- TODO: Fill this field
            fare = Nothing,
            totalFare = Nothing,
            chargeableDistance = Nothing,
            vehicleVariant = rideBooking.vehicleVariant,
            createdAt = now,
            updatedAt = now,
            ..
          }
processEvent (OnUpdate.RideStarted rsEvent) = do
  let bppBookingId = Id rsEvent.order_id
      bppRideId = Id rsEvent.ride_id
  rideBooking <- QRB.findByBPPBookingId bppBookingId >>= fromMaybeM RideBookingDoesNotExist
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM RideDoesNotExist
  unless (rideBooking.status == SRB.TRIP_ASSIGNED) $ throwError (RideBookingInvalidStatus $ show rideBooking.status)
  unless (ride.status == SRide.NEW) $ throwError (RideInvalidStatus $ show ride.status)
  DB.runSqlDBTransaction $ do
    QRide.updateStatus ride.id SRide.INPROGRESS
  Notify.notifyOnRideStarted rideBooking ride
processEvent (OnUpdate.RideCompleted rcEvent) = do
  let bppBookingId = Id rcEvent.order_id
      bppRideId = Id rcEvent.ride_id
  rideBooking <- QRB.findByBPPBookingId bppBookingId >>= fromMaybeM RideBookingDoesNotExist
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM RideDoesNotExist
  unless (rideBooking.status == SRB.TRIP_ASSIGNED) $ throwError (RideBookingInvalidStatus $ show rideBooking.status)
  unless (ride.status == SRide.INPROGRESS) $ throwError (RideInvalidStatus $ show ride.status)
  let updRide =
        ride{status = SRide.COMPLETED,
             fare = Just $ realToFrac rcEvent.fare.value,
             totalFare = Just $ realToFrac rcEvent.total_fare.value,
             chargeableDistance = Just $ realToFrac rcEvent.chargeable_distance
            }
  DB.runSqlDBTransaction $ do
    QRB.updateStatus rideBooking.id SRB.COMPLETED
    QRide.updateMultiple updRide.id updRide
  Notify.notifyOnRideCompleted rideBooking ride
processEvent (OnUpdate.RideBookingCancelled tcEvent) = do
  let bppRideBookingId = Id $ tcEvent.order_id
  rideBooking <- QRB.findByBPPBookingId bppRideBookingId >>= fromMaybeM RideBookingDoesNotExist
  unless (isRideBookingCancellable rideBooking) $
    throwError (RideBookingInvalidStatus (show rideBooking.status))
  mbRide <- QRide.findByRBId rideBooking.id
  let cancellationSource = tcEvent.cancellation_reason_id
  let searchRequestId = rideBooking.requestId
  logTagInfo ("txnId-" <> getId searchRequestId) ("Cancellation reason " <> show cancellationSource)
  DB.runSqlDBTransaction $ do
    QRB.updateStatus rideBooking.id SRB.CANCELLED
    whenJust mbRide $ \ride -> QRide.updateStatus ride.id SRide.CANCELLED
    unless (cancellationSource == OnUpdate.ByUser) $
      QRCR.create $ SRCR.RideCancellationReason rideBooking.id cancellationSource Nothing Nothing
  -- notify customer
  Notify.notifyOnRideBookingCancelled rideBooking cancellationSource

isRideBookingCancellable :: SRB.RideBooking -> Bool
isRideBookingCancellable ride =
  ride.status `elem` [SRB.CONFIRMED, SRB.TRIP_ASSIGNED]
