module Domain.Action.Internal.DriverLiveActivity where

import Environment
import Kernel.Beam.Functions as B
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import qualified Data.Text as Text
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Notifications as Notify
import qualified Kernel.External.Notification.FCM.Types as FCMType
import qualified SharedLogic.Ride as SRide

data DriverLiveActivityReq = DriverLiveActivityReq
  { bppRideId :: Text,
    driverLat :: Double,
    driverLon :: Double,
    ridePhase :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

driverLiveActivityHandler :: DriverLiveActivityReq -> Flow APISuccess
driverLiveActivityHandler req = do
  ride <- B.runInReplica $ QRide.findByBPPRideId (Id req.bppRideId) >>= fromMaybeM (RideDoesNotExist req.bppRideId)
  booking <- B.runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  person <- B.runInReplica $ QPerson.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  logDebug $ "[LiveActivity] received req bppRideId=" <> req.bppRideId <> " phase=" <> req.ridePhase <> " riderId=" <> booking.riderId.getId
  case person.liveActivityToken of
    Nothing -> do
      logDebug $ "[LiveActivity] no liveActivityToken for person=" <> person.id.getId <> ", skipping"
      pure Success
    Just liveToken -> do
      let driverLoc = LatLong req.driverLat req.driverLon
      mbEtaSeconds <- if req.ridePhase == "NEW"
        then do
          (mbEtaMinutes, dist) <- SRide.calculatePickupETA ride booking driverLoc
          logDebug $ "[LiveActivity] NEW phase: pickupEtaMinutes=" <> show mbEtaMinutes <> " distToPickup=" <> show dist
          pure $ (* 60) <$> mbEtaMinutes
        else do
          now <- getCurrentTime
          let mbEta = case ride.estimatedEndTimeRange of
                Just range -> Just $ max 0 $ getSeconds $ nominalDiffTimeToSeconds $ diffUTCTime range.end now
                Nothing    -> Nothing
          logDebug $ "[LiveActivity] INPROGRESS phase: estimatedEndTimeRange=" <> show ride.estimatedEndTimeRange <> " etaSeconds=" <> show mbEta
          pure mbEta
      let etaText = case mbEtaSeconds of
            Nothing -> Nothing
            Just s ->
              let mins = ceiling (fromIntegral s / 60.0 :: Double) :: Int
              in Just $ if mins < 1 then "< 1 min" else show mins <> " min"
          actStatus = if req.ridePhase == "NEW" then "ASSIGNED" else "ONRIDE"
          liveReq =
            FCMType.LiveActivityReq
              { liveActivityToken = liveToken,
                liveActivityReqType = "update",
                liveActivityNotificationType = "DRIVER_LOCATION_UPDATE",
                liveActivityContentState =
                  FCMType.LiveActivityContentState
                    { activityStatus = actStatus,
                      driverInfo =
                        Just $
                          FCMType.DriverInfo
                            { rideOtp = Nothing,
                              driverName = Nothing,
                              distanceLeft = etaText,
                              totalDistance = Nothing,
                              driverNumber = Nothing,
                              driverProfile = Nothing
                            },
                      bookingInfo = Nothing,
                      timerDuration = Nothing,
                      customMessage = Nothing
                    },
                liveActivityApnsPriority = "10"
              }
      logDebug $ "[LiveActivity] sending FCM liveActivity: status=" <> actStatus <> " etaText=" <> show etaText <> " timerDuration=" <> show mbEtaSeconds <> " tokenLen=" <> show (Text.length liveToken)
      Notify.sendLiveActivityLocationUpdate person liveReq
      pure Success
