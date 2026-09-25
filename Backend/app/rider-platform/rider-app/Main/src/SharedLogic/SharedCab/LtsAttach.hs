-- | The cab's route attach in LTS. LTS is enrichment only; the session is the truth, so:
-- attach failures fail the caller (a selected cab must be trackable), detach failures are logged
-- (a lingering `route:{code}` field is harmless because listing starts from ACTIVE sessions).
module SharedLogic.SharedCab.LtsAttach
  ( LtsFlow,
    attach,
    detach,
    withAttach,
  )
where

import Data.List (sortOn)
import Data.Ord (Down (..))
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common
import Kernel.Utils.Forkable (runWithFallbackAndTimeout)
import qualified SharedLogic.External.LocationTrackingService.Flow as LTS
import SharedLogic.External.LocationTrackingService.Types
import SharedLogic.SharedCab.SessionState (Session (..))
import qualified Storage.CachedQueries.IntegratedBPPConfig as CQIBC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import Tools.Error

type LtsFlow m r c =
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    CoreMetrics m,
    HasLocationService m r,
    HasShortDurationRetryCfg r c,
    HasRequestId r,
    MonadReader r m,
    Forkable m
  )

-- Each LTS step runs under the plate lock (Session.lockTtlSec 30): waiting is cut off here so detach + attach +
-- restore stay well inside it. The HTTP call itself can't be cancelled and finishes in the background.
ltsStepSec :: Int
ltsStepSec = 6

bounded :: (LtsFlow m r c, MonadThrow m) => Text -> m () -> m ()
bounded tag action = runWithFallbackAndTimeout tag [()] ltsStepSec (const True) (const action)

busRideInfo :: Session -> LatLong -> RideInfo
busRideInfo s destination =
  Bus
    BusRideInfo
      { routeCode = s.routeCode,
        busNumber = s.vehicleNumber,
        destination,
        routeLongName = Nothing,
        driverName = Nothing,
        groupId = Just s.merchantOperatingCityId.getId
      }

-- LTS keys the ride record by the driver's merchant in driver-app.
ltsMerchantId :: LtsFlow m r c => Session -> m Text
ltsMerchantId s = (.driverOfferMerchantId) <$> (CQM.findById s.merchantId >>= fromMaybeM (MerchantNotFound s.merchantId.getId))

-- | rideStart on the session's route, keyed by its vehicle_trip id. Throws if LTS or the route's stops are unavailable.
attach :: (LtsFlow m r c, MonadThrow m) => Session -> m ()
attach s = bounded "sharedCab:ltsAttach" $ do
  merchantId <- ltsMerchantId s
  ibc <- CQIBC.findById s.integratedBppConfigId >>= fromMaybeM IntegratedBPPConfigNotFound
  stops <- OTPRest.getRouteStopMappingByRouteCode s.routeCode ibc
  lastStop <- fromMaybeM (RouteNotFound s.routeCode) $ listToMaybe (sortOn (Down . (.sequenceNum)) stops)
  LTS.rideStart s.vehicleTripId.getId $
    RideStartReq {merchantId, driverId = s.driverId, rideInfo = Just (busRideInfo s lastStop.stopPoint)}

-- | rideEnd carrying the route code — without it LTS keeps the cab in `route:{code}`. Never throws.
detach :: (LtsFlow m r c, MonadThrow m) => Session -> m ()
detach s =
  withTryCatch "sharedCab:ltsDetach" (bounded "sharedCab:ltsDetach" $ ltsMerchantId s >>= end) >>= \case
    Left err -> logError $ "LTS rideEnd failed for " <> s.vehicleNumber <> " on " <> s.routeCode <> ": " <> show err
    Right () -> pure ()
  where
    -- LTS end reads only routeCode/busNumber from the ride info and echoes lat/lon back, so no stop lookup here.
    nowhere = LatLong 0 0
    end merchantId =
      LTS.rideEnd s.vehicleTripId.getId $
        RideEndReq {lat = 0, lon = 0, merchantId, driverId = s.driverId, rideInfo = Just (busRideInfo s nowhere)}

-- | Attach `new` (after detaching `old`: rideEnd clears LTS's per-driver ride record, so it must come first), then
-- run `persist`. Any failure puts the LTS attach back where the session still is and rethrows.
withAttach :: (LtsFlow m r c, MonadCatch m) => Maybe Session -> Session -> m a -> m a
withAttach old new persist = do
  traverse_ detach old
  attach new `onException` restore
  persist `onException` (detach new >> restore)
  where
    restore = traverse_ (withTryCatch "sharedCab:ltsReattach" . attach) old
