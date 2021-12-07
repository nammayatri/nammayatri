module Product.Update where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Cabs.API.OnUpdate as OnUpdate
import qualified Beckn.Types.Core.Cabs.OnUpdate as OnUpdate
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import EulerHS.Prelude hiding (state)
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import Types.Error
import qualified Types.Storage.Ride as SRide
import qualified Types.Storage.RideBooking as SRB
import Utils.Common

onUpdate ::
  SignatureAuthResult ->
  OnUpdate.OnUpdateReq ->
  FlowHandler AckResponse
onUpdate _org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    -- TODO: Verify api key here
    logTagInfo "on_update req" (show req)
    validateContext req.context
    case req.contents of
      Left err -> logTagError "on_update req" $ "on_update error: " <> show err
      Right msg -> processOrder msg.order
    return Ack

processOrder :: DBFlow m r => OnUpdate.RideOrder -> m ()
processOrder (OnUpdate.TripAssigned taOrder) = do
  let bppBookingId = Id taOrder.id
  rideBooking <- QRB.findByBPPBookingId bppBookingId >>= fromMaybeM RideBookingDoesNotExist
  unless (rideBooking.status == SRB.CONFIRMED) $ throwError (RideBookingInvalidStatus $ show rideBooking.status)
  ride <- buildRide rideBooking
  DB.runSqlDBTransaction $ do
    QRB.updateStatus rideBooking.id SRB.TRIP_ASSIGNED
    QRide.create ride
  where
    buildRide :: MonadFlow m => SRB.RideBooking -> m SRide.Ride
    buildRide rideBooking = do
      guid <- generateGUID
      shortId <- generateShortId
      now <- getCurrentTime
      let fulfillment = taOrder.fulfillment
          bppRideId = Id taOrder.fulfillment.id
          otp = fulfillment.otp
          driverName = fulfillment.agent.name
          driverMobileNumber = fulfillment.agent.phone
          driverRating = fulfillment.agent.rating
          driverRegisteredAt = fulfillment.agent.registered_at
          vehicleNumber = fulfillment.vehicle.registration
          vehicleColor = fulfillment.vehicle.color
          vehicleModel = fulfillment.vehicle.model
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
processOrder (OnUpdate.RideStarted rsOrder) = do
  let bppBookingId = Id rsOrder.id
      bppRideId = Id rsOrder.fulfillment.id
  rideBooking <- QRB.findByBPPBookingId bppBookingId >>= fromMaybeM RideBookingDoesNotExist
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM RideDoesNotExist
  unless (rideBooking.status == SRB.TRIP_ASSIGNED) $ throwError (RideBookingInvalidStatus $ show rideBooking.status)
  unless (ride.status == SRide.NEW) $ throwError (RideInvalidStatus $ show ride.status)
  DB.runSqlDBTransaction $ do
    QRide.updateStatus ride.id SRide.INPROGRESS
processOrder (OnUpdate.RideCompleted rcOrder) = do
  let bppBookingId = Id rcOrder.id
      bppRideId = Id rcOrder.fulfillment.id
  rideBooking <- QRB.findByBPPBookingId bppBookingId >>= fromMaybeM RideBookingDoesNotExist
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM RideDoesNotExist
  unless (rideBooking.status == SRB.TRIP_ASSIGNED) $ throwError (RideBookingInvalidStatus $ show rideBooking.status)
  unless (ride.status == SRide.INPROGRESS) $ throwError (RideInvalidStatus $ show ride.status)
  let updRide =
        ride{status = SRide.COMPLETED,
             totalFare = Just $ realToFrac rcOrder.payment.params.amount,
             chargeableDistance = Just rcOrder.fulfillment.chargeable_distance
            }
  DB.runSqlDBTransaction $ do
    QRB.updateStatus rideBooking.id SRB.COMPLETED
    QRide.updateMultiple updRide.id updRide