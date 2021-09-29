module Product.Update where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Core.API.Update
import Beckn.Types.Core.Ack
import Beckn.Types.Core.DecimalValue
import Beckn.Types.Id
import qualified Beckn.Types.Mobility.Order as Mobility
import Beckn.Types.Mobility.Trip
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Data.Text as T
import EulerHS.Prelude hiding (state)
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import Types.Error
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.Ride as SRide
import qualified Types.Storage.RideBooking as SRB
import Utils.Common

onUpdate ::
  SignatureAuthResult Organization.Organization ->
  OnUpdateReq ->
  FlowHandler AckResponse
onUpdate _org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    -- TODO: Verify api key here
    logTagInfo "on_update req" (show req)
    validateContext "on_update" $ req.context
    case req.contents of
      Right msg -> do
        let trip = msg.order.trip
            quoteId = Id $ msg.order.id
            mbState = msg.order.state
        rideBooking <- QRB.findByQuoteId quoteId >>= fromMaybeM RideBookingDoesNotExist
        mbRide <- QRide.findByRBId rideBooking.id
        let statusTransaction =
              whenJust mbState $ \state -> do
                unless (state == Mobility.INPROGRESS) $
                  QRB.updateStatus rideBooking.id $ convertRBStatus state

        mbBuildRideTransaction <- do
          case mbState of
            Nothing -> return $ return ()
            Just state ->
              if state == Mobility.TRIP_ASSIGNED && isNothing mbRide
                then buildRide trip rideBooking <&> QRide.create
                else return $ return ()

        let updateTransaction =
              whenJust mbRide $ \ride -> do
                let uRide =
                      ride{status = maybe ride.status convertRideStatus mbState,
                           finalPrice =
                             trip >>= fare >>= (.computed_value) >>= convertDecimalValueToAmount,
                           finalDistance =
                             fromMaybe 0 $ trip >>= (.route) >>= (.edge.distance.computed_value)
                          }
                QRide.updateMultiple (uRide.id) uRide
        DB.runSqlDBTransaction $ do
          statusTransaction
          mbBuildRideTransaction
          updateTransaction
      Left err -> logTagError "on_update req" $ "on_update error: " <> show err
    return Ack
  where
    convertRBStatus = \case
      Mobility.CONFIRMED -> SRB.CONFIRMED
      Mobility.TRIP_ASSIGNED -> SRB.TRIP_ASSIGNED
      Mobility.INPROGRESS -> SRB.TRIP_ASSIGNED
      Mobility.COMPLETED -> SRB.COMPLETED
      Mobility.CANCELLED -> SRB.CANCELLED

    convertRideStatus = \case
      Mobility.CONFIRMED -> SRide.NEW
      Mobility.TRIP_ASSIGNED -> SRide.NEW
      Mobility.INPROGRESS -> SRide.INPROGRESS
      Mobility.COMPLETED -> SRide.COMPLETED
      Mobility.CANCELLED -> SRide.CANCELLED

    buildRide :: MonadFlow m => Maybe Trip -> SRB.RideBooking -> m SRide.Ride
    buildRide mbTrip rideBooking = do
      guid <- generateGUID
      shortId <- generateShortId
      now <- getCurrentTime
      otp <- mbTrip <&> (.id) & fromMaybeM (InternalError "Otp is not present.")
      driverName <- mbTrip >>= (.driver) <&> (.name.given_name) & fromMaybeM (InternalError "Driver name is not present.")
      driverMobileNumber <- mbTrip >>= (.driver) <&> (.phones) >>= listToMaybe & fromMaybeM (InternalError "Driver mobile number is not present.")
      driverRegisteredAt <- mbTrip >>= (.driver) <&> (.registeredAt) & fromMaybeM (InternalError "Driver registration date is not present.")
      vehicleNumber <- mbTrip >>= (.vehicle) >>= (.registration) <&> (.number) & fromMaybeM (InternalError "Vehicle registration number is not present.")
      vehicleColor <- mbTrip >>= (.vehicle) >>= (.color) & fromMaybeM (InternalError "Vehicle color is not present.")
      vehicleModel <- mbTrip >>= (.vehicle) >>= (.model) & fromMaybeM (InternalError "Vehicle model is not present.")
      return
        SRide.Ride
          { id = guid,
            bookingId = rideBooking.id,
            driverRating = mbTrip >>= (.driver) >>= (.rating) <&> (.value) >>= readMaybe . T.unpack,
            status = SRide.NEW,
            trackingUrl = "UNKNOWN", -- TODO: Fill this field
            finalPrice = Nothing,
            finalDistance = 0,
            createdAt = now,
            updatedAt = now,
            ..
          }