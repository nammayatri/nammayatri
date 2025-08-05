{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.StopEvents where

import Data.Foldable ()
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideStatus as DRide
import qualified Domain.Types.StopInformation as DSI
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.CacheFlow
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import qualified Storage.CachedQueries.Merchant as QMerc
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.LocationMapping as QLM
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.StopInformation as QSI

data StopEventsReq = StopEventsReq
  { action :: StopAction,
    rideId :: Id DRide.BPPRide, ----------bppRideId
    stopOrder :: Int,
    waitingTimeStart :: UTCTime,
    waitingTimeEnd :: Maybe UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data StopAction = Depart | Arrive
  deriving (Generic, ToJSON, FromJSON, ToSchema)

stopEvents ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["internalAPIKey" ::: Text]
  ) =>
  Maybe Text ->
  StopEventsReq ->
  m APISuccess
stopEvents apiKey StopEventsReq {..} = do
  internalAPIKey <- asks (.internalAPIKey)
  unless (Just internalAPIKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  ride <- B.runInReplica $ QRide.findByBPPRideId rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- B.runInReplica $ B.runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  person <- B.runInReplica $ QP.findById booking.riderId >>= fromMaybeM (PersonDoesNotExist booking.riderId.getId)
  _merchant <- QMerc.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
  void $ validateRequest ride
  stopsLM <- QLM.getLatestStopsByEntityId ride.id.getId
  stopLM <- find (\lm -> lm.order == stopOrder) stopsLM & fromMaybeM (InvalidRequest $ "Stop LM for order " <> show stopOrder <> "doesn't exist for ride " <> ride.id.getId)
  case action of
    Depart -> do
      QSI.updateByStopOrderAndRideId waitingTimeEnd ride.id stopOrder
      pure Success
    Arrive -> do
      id <- generateGUID
      now <- getCurrentTime
      let stopInfo =
            DSI.StopInformation
              { stopId = stopLM.locationId,
                stopOrder = stopOrder,
                waitingTimeStart = waitingTimeStart,
                waitingTimeEnd = Nothing,
                rideId = ride.id,
                createdAt = now,
                updatedAt = now,
                id,
                merchantOperatingCityId = ride.merchantOperatingCityId,
                merchantId = ride.merchantId
              }
      QSI.create stopInfo
      pure Success

validateRequest :: (MonadFlow m) => DRide.Ride -> m ()
validateRequest ride = do
  -- unless (ratingValue `elem` [1 .. 5]) $ throwError InvalidRatingValue
  unless (ride.status == DRide.INPROGRESS) $ throwError (RideInvalidStatus ("Stop Events available only for inprogress rides. RideId : " <> ride.id.getId))
