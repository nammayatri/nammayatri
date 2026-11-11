-- | rider-app's reads and writes of a shared cab's run: the plate's ACTIVE trip, to rebuild a session it lost from Redis, and the end
-- of a run rider-app closed itself. The caller authenticates with the merchant's internal api key.
module Domain.Action.Internal.SharedCabTrip
  ( sharedCabActiveTrip,
    expireSharedCabTrip,
  )
where

import API.Types.UI.SharedCabInternal (SharedCabTrip (..))
import Domain.Types.IntegratedBPPConfig (PlatformType (..))
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.VehicleTrip as DVT
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.IntegratedBPPConfig (findIntegratedBPPConfig)
import qualified SharedLogic.SharedCab.Trip as Trip
import qualified Storage.CachedQueries.Merchant as QM
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
