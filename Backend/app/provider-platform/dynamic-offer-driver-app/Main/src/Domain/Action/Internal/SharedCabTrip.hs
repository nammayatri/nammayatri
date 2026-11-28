-- | rider-app's reads and writes of a shared cab's run: the plate's ACTIVE trip, to rebuild a session it lost from Redis, the end
-- of a run rider-app closed itself, and the plate assigned to a materialised shared run. The caller authenticates with the merchant's internal api key.
module Domain.Action.Internal.SharedCabTrip
  ( sharedCabActiveTrip,
    expireSharedCabTrip,
    sharedRunVehicle,
  )
where

import API.Types.UI.SharedCabInternal (SharedCabTrip (..), SharedRunVehicle (..))
import Data.Maybe (listToMaybe)
import Data.Time (Day)
import qualified Domain.Types.Booking as DRB
import Domain.Types.IntegratedBPPConfig (PlatformType (..))
import qualified Domain.Types.IntegratedBPPConfig as Fields
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideDetails as DRideDetails
import qualified Domain.Types.VehicleTrip as DVT
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.GtfsDataServer.Types (gtfsLocalId)
import SharedLogic.IntegratedBPPConfig (findIntegratedBPPConfig)
import qualified SharedLogic.SharedCab.Trip as Trip
import SharedLogic.SharedRun.Materialise (runTransactionId)
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.IntegratedBPPConfig as QIBC
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideDetails as QRideDetails
import qualified Storage.Queries.VehicleTrip as QVT

authMerchant :: Id DM.Merchant -> Maybe Text -> Flow ()
authMerchant merchantId apiKey = do
  mbMerchant <- QM.findById merchantId
  case mbMerchant of
    Just m | Just m.internalApiKey == apiKey -> pure ()
    _ -> throwError $ AuthBlocked "Invalid BPP internal api key"

sharedCabActiveTrip :: Id DM.Merchant -> Text -> Maybe Text -> Flow (Maybe SharedCabTrip)
sharedCabActiveTrip merchantId vehicleNumber apiKey = do
  authMerchant merchantId apiKey
  mbTrip <- mfilter ((== merchantId) . (.merchantId)) <$> QVT.findActiveByVehicleNumber vehicleNumber
  forM mbTrip $ \trip -> do
    integratedBPPConfig <- findIntegratedBPPConfig (Just trip.integratedBppConfigId) trip.merchantOperatingCityId "SHARED_CAB" APPLICATION
    pure
      SharedCabTrip
        { id = trip.id.getId,
          driverId = trip.driverId.getId,
          vehicleNumber = trip.vehicleNumber,
          agencyId = integratedBPPConfig.agencyKey,
          routeCode = trip.routeCode,
          capacity = trip.capacity,
          walkupCount = trip.walkupCount,
          startedAt = trip.startedAt
        }

-- | rider-app expired the run (the cab went silent). Closes it only while it is still the plate's ACTIVE trip: a later run is left alone.
expireSharedCabTrip :: Id DM.Merchant -> Text -> Maybe HighPrecMoney -> Maybe Text -> Flow APISuccess
expireSharedCabTrip merchantId tripId earnings apiKey = do
  authMerchant merchantId apiKey
  trip <- QVT.findById (Id tripId) >>= fromMaybeM (InvalidRequest $ "No shared-cab trip " <> tripId)
  unless (trip.merchantId == merchantId) $ throwError $ InvalidRequest ("No shared-cab trip " <> tripId)
  Trip.withTripLock trip.vehicleNumber $ do
    active <- QVT.findActiveByVehicleNumber trip.vehicleNumber
    when (((.id) <$> active) == Just trip.id) $ Trip.closeTrip DVT.SESSION_TIMEOUT earnings trip
  pure Success

-- | The plate of the vehicle assigned to a materialised shared run, found the way the materialiser keys the run: the city's first
-- config of the feed, the GTFS trip and the service day. Nothing while the run is not materialised or not yet assigned, or when
-- its ride was cancelled.
sharedRunVehicle :: Id DM.Merchant -> Context.City -> Text -> Day -> Text -> Maybe Text -> Flow (Maybe SharedRunVehicle)
sharedRunVehicle merchantId city feedKey serviceDate tripId apiKey = do
  authMerchant merchantId apiKey
  mbMoc :: Maybe DMOC.MerchantOperatingCity <- CQMOC.findByMerchantIdAndCity merchantId city
  mbConfig <- maybe (pure Nothing) (fmap listToMaybe . QIBC.findAllByFeedKeyAndCity feedKey . (.id)) mbMoc
  case mbConfig of
    Nothing -> pure Nothing
    Just integratedBppConfig -> do
      -- the materialiser keys a run by the trip's local id (the feed prefix stripped)
      let transactionId = runTransactionId integratedBppConfig.id (gtfsLocalId tripId) serviceDate
      mbBooking :: Maybe DRB.Booking <- QRB.findByTransactionId transactionId
      mbRide :: Maybe DRide.Ride <- maybe (pure Nothing) (QRide.findActiveByRBId . (.id)) mbBooking
      mbRideDetails :: Maybe DRideDetails.RideDetails <- maybe (pure Nothing) (QRideDetails.findById . (.id)) mbRide
      pure $ (\rideDetails -> SharedRunVehicle {vehicleNumber = rideDetails.vehicleNumber}) <$> mbRideDetails
