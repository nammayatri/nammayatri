{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.OnTrack
  ( onTrack,
    OnTrackReq (..),
    TrackingLocation (..),
    validateRequest,
  )
where

import qualified Beckn.OnDemand.Utils.Common as Common
import Domain.Types.Ride
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Beckn.Gps as Gps
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide
import Tools.Error

data OnTrackReq = OnTrackReq
  { bppRideId :: Id BPPRide,
    trackUrl :: Maybe BaseUrl,
    trackingLocation :: Maybe TrackingLocation
  }

data TrackingLocation = TrackingLocation
  { gps :: Gps.Gps,
    updatedAt :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data ValidatedOnTrackReq = ValidatedOnTrackReq
  { bppRideId :: Id BPPRide,
    trackUrl :: Maybe BaseUrl,
    ride :: Ride,
    isValueAddNP :: Bool,
    trackingLocation :: Maybe TrackingLocation
  }

onTrack :: (CacheFlow m r, EsqDBFlow m r) => ValidatedOnTrackReq -> m ()
onTrack ValidatedOnTrackReq {..} = do
  if isValueAddNP
    then do
      whenJust trackUrl $ \trackUrl' -> do
        void $ QRide.updateTrackingUrl ride.id trackUrl'
    else do
      whenJust trackingLocation $ \trackingLocation' -> do
        void $ Hedis.setExp (Common.mkRideTrackingRedisKey ride.id.getId) trackingLocation' 10

validateRequest :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => OnTrackReq -> m ValidatedOnTrackReq
validateRequest OnTrackReq {..} = do
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId:" <> bppRideId.getId)
  booking <- B.runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  isValueAddNP <- CQVAN.isValueAddNP booking.providerId
  if isValueAddNP && isNothing trackUrl
    then throwError $ InvalidRequest "TrackUrl is required for ValueAddNP"
    else
      if not isValueAddNP && isNothing trackingLocation
        then throwError $ InvalidRequest "TrackingLocation is required for non ValueAddNP"
        else
          return $
            ValidatedOnTrackReq
              { ..
              }
