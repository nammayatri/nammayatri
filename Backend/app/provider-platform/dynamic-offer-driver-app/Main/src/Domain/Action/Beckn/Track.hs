{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.Track
  ( track,
    DTrackReq (..),
    DTrackRes (..),
  )
where

import Data.Maybe
import qualified Domain.Types.Booking as DBooking
import Domain.Types.DriverLocation
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Ride as DRide
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Version (CloudType (..))
import Kernel.Utils.Common
import qualified SharedLogic.External.LocationTrackingService.API.DriversLocation as DriversLocationAPI
import SharedLogic.External.LocationTrackingService.Types
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide

newtype DTrackReq = TrackReq
  { bookingId :: Id DBooking.Booking
  }

data DTrackRes = TrackRes
  { url :: BaseUrl,
    transporter :: DM.Merchant,
    isRideCompleted :: Bool,
    driverLocation :: Maybe DriverLocation,
    isValueAddNP :: Bool
  }
  deriving (Generic, Show)

track ::
  (CacheFlow m r, EsqDBFlow m r, HasFlowEnv m r '["ltsCfg" ::: LocationTrackingeServiceConfig, "cloudType" ::: Maybe CloudType], HasShortDurationRetryCfg r c) =>
  Id DM.Merchant ->
  DTrackReq ->
  m DTrackRes
track transporterId req = do
  transporter <-
    QM.findById transporterId
      >>= fromMaybeM (MerchantNotFound transporterId.getId)
  ride <- QRide.findOneByBookingId req.bookingId >>= fromMaybeM (RideDoesNotExist req.bookingId.getId)
  booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  let transporterId' = booking.providerId
  isValueAddNP <- CQVAN.isValueAddNP booking.bapId
  unless (transporterId' == transporterId) $ throwError AccessDenied
  let isRideCompleted = (\status -> status `elem` [DRide.COMPLETED, DRide.CANCELLED]) ride.status
  (driverLocation :: Maybe DriverLocation) <-
    if not isValueAddNP
      then do
        driverLocations <- callMultiCloudDriverLocation ride
        let resLoc = listToMaybe $ sortBy (comparing (Down . (.coordinatesCalculatedAt))) driverLocations
        logTagDebug ("rideId:-" <> show ride.id) $ "track driverLocation:-" <> show resLoc
        return resLoc
      else return Nothing
  return $
    TrackRes
      { url = ride.trackingUrl,
        ..
      }

callMultiCloudDriverLocation :: (CacheFlow m r, EsqDBFlow m r, CoreMetrics m, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LocationTrackingeServiceConfig, "cloudType" ::: Maybe CloudType], HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m, Forkable m) => DRide.Ride -> m [DriverLocation]
callMultiCloudDriverLocation ride = do
  cloudType <- asks (.cloudType)
  ltsCfg <- asks (.ltsCfg)
  let req = DriversLocationReq {driverIds = [ride.driverId]}
  case cloudType of
    Nothing -> callBothClouds ltsCfg.url ltsCfg.secondaryUrl req
    Just UNAVAILABLE -> callBothClouds ltsCfg.url ltsCfg.secondaryUrl req
    Just ct
      | Just ct == ride.cloudType -> callDriverLocationAPI ltsCfg.url req
      | otherwise ->
        maybe
          (logError "Secondary URL not configured for cross-cloud tracking" >> pure [])
          (`callDriverLocationAPI` req)
          ltsCfg.secondaryUrl
  where
    callBothClouds primaryUrl mbSecondaryUrl req = do
      primaryAwaitable <- awaitableFork "primaryLTS" $ callDriverLocationAPI primaryUrl req
      mbSecondaryAwaitable <- forM mbSecondaryUrl $ \url -> awaitableFork "secondaryLTS" $ callDriverLocationAPI url req
      primaryRes <-
        L.await Nothing primaryAwaitable >>= \case
          Left err -> throwError $ InternalError $ "Failed to call driversLocation API for primary url: " <> show primaryUrl <> ", error: " <> show err
          Right result -> pure result
      secondaryRes <- maybe (pure []) handleSecondaryResult mbSecondaryAwaitable
      pure $ primaryRes <> secondaryRes
      where
        handleSecondaryResult awaitable =
          L.await Nothing awaitable >>= \case
            Left err -> logError ("Failed to call driversLocation API for secondary url, error: " <> show err) >> pure []
            Right result -> pure result

    callDriverLocationAPI url req =
      withShortRetry $
        callAPI url (DriversLocationAPI.driversLocation req) "driversLocation" DriversLocationAPI.locationTrackingServiceAPI
          >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_DRIVERS_LOCATION_API") url)
