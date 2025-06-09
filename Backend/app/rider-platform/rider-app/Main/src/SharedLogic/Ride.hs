module SharedLogic.Ride where

import qualified Beckn.OnDemand.Utils.Common as Common
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text
import qualified Domain.Action.Beckn.OnTrack as OnTrack
import Domain.Types.Ride
import qualified Domain.Types.Ride as SRide
import Kernel.Beam.Functions as B
import Kernel.External.Encryption
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude hiding (HasField)
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto hiding (isNothing)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.JourneyLeg.Types
import qualified SharedLogic.CallBPP as CallBPP
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Maps as MapSearch
import TransactionLogs.Types

data GetDriverLocResp = GetDriverLocResp
  { lat :: Double,
    lon :: Double,
    pickupStage :: Maybe JourneyLegStatus,
    lastUpdate :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

getDriverLoc ::
  ( CacheFlow m r,
    EncFlow m r,
    EsqDBFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl, "smsCfg" ::: SmsConfig],
    EsqDBReplicaFlow m r,
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasLongDurationRetryCfg r c
  ) =>
  Id SRide.Ride ->
  m GetDriverLocResp
getDriverLoc rideId = do
  ride <- B.runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  when
    (ride.status == COMPLETED || ride.status == CANCELLED)
    $ throwError $ RideInvalidStatus ("Cannot track this ride" <> Text.pack (show ride.status))
  booking <- B.runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  isValueAddNP <- CQVAN.isValueAddNP booking.providerId
  res <-
    if isValueAddNP && isJust ride.trackingUrl
      then CallBPP.callGetDriverLocation ride.trackingUrl
      else do
        withLongRetry $ CallBPP.callTrack booking ride
        trackingLoc :: OnTrack.TrackingLocation <- Redis.get (Common.mkRideTrackingRedisKey ride.id.getId) >>= fromMaybeM (InvalidRequest "Driver location not updated")
        return $
          CallBPP.GetLocationRes
            { currPoint = MapSearch.LatLong {lat = trackingLoc.gps.lat, lon = trackingLoc.gps.lon},
              lastUpdate = trackingLoc.updatedAt
            }
  return $
    GetDriverLocResp
      { lat = res.currPoint.lat,
        lon = res.currPoint.lon,
        lastUpdate = res.lastUpdate,
        pickupStage = booking.journeyLegStatus
      }
