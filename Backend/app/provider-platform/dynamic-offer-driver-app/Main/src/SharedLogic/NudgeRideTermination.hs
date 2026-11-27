module SharedLogic.NudgeRideTermination
  ( endActiveNudgeRideForDriver,
  )
where

import qualified Domain.Action.UI.Ride.EndRide as RideEnd
import qualified Domain.Types.Booking as DBooking
import Domain.Types.Person (Person)
import qualified Domain.Types.Ride as DRide
import Environment
import Kernel.External.Maps (LatLong (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide

endActiveNudgeRideForDriver :: Id Person -> Flow ()
endActiveNudgeRideForDriver driverId = fork "endNudgeRideOnNewAssign" $ do
  let nudgeRideRedisKey = "nudgeRide:" <> driverId.getId
  mbNudgeRideId <- Redis.safeGet @Text nudgeRideRedisKey
  Redis.del nudgeRideRedisKey
  whenJust mbNudgeRideId $ \nudgeRideIdText -> do
    let nudgeRideId = Id nudgeRideIdText :: Id DRide.Ride
    mbNudgeRide <- QRide.findById nudgeRideId
    whenJust mbNudgeRide $ \nudgeRide -> do
      mbNudgeBooking <- QRB.findById nudgeRide.bookingId
      whenJust mbNudgeBooking $ \nudgeBooking ->
        when (nudgeBooking.bookingType == DBooking.NudgeRide) $ do
          locResp <- LF.driverLocation nudgeRideId nudgeBooking.providerId nudgeRide.driverId
          let mbLastPoint = listToMaybe locResp.loc <&> \l -> LatLong {lat = l.lat, lon = l.lon}
          shandle <- RideEnd.buildEndRideHandle nudgeBooking.providerId nudgeRide.merchantOperatingCityId (Just nudgeRideId) False
          void $
            RideEnd.cronJobEndRide shandle nudgeRideId $
              RideEnd.CronJobEndRideReq {point = mbLastPoint, merchantId = nudgeBooking.providerId}
