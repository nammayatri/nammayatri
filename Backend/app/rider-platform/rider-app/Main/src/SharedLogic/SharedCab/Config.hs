module SharedLogic.SharedCab.Config
  ( SharedCabTunables (..),
    defaultTunables,
    tunablesFrom,
    getTunables,
  )
where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.RiderConfig as DRC
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))

-- | `05` §10 and `04` §3 tunables, all in seconds or metres. Each has a nullable rider_config column
-- `sharedCab<Field>`; NULL (or no row) takes the default.
data SharedCabTunables = SharedCabTunables
  { allocationWindowSec :: Int,
    atStopRadiusM :: Int,
    walkBufferSec :: Int,
    standTimerSec :: Int,
    movingTimerSec :: Int,
    maxAttempts :: Int,
    fallbackAfterSec :: Int,
    noCabGraceSec :: Int,
    findingTimeoutSec :: Int,
    tickSec :: Int,
    ltsMaxAgeSec :: Int,
    autoEndAfterDropSec :: Int,
    degradedTimeoutSec :: Int,
    offRouteMeters :: Int,
    offRouteSec :: Int,
    boardProximityM :: Int,
    boardAttemptsPer10Min :: Int,
    noLocationSpotBookingsPerVehiclePerDay :: Int
  }
  deriving (Show, Eq, Generic)

defaultTunables :: SharedCabTunables
defaultTunables =
  SharedCabTunables
    { allocationWindowSec = 8 * 60,
      atStopRadiusM = 100,
      walkBufferSec = 60,
      standTimerSec = 180,
      movingTimerSec = 90,
      maxAttempts = 2,
      fallbackAfterSec = 10 * 60,
      noCabGraceSec = 2 * 60,
      findingTimeoutSec = 20 * 60,
      tickSec = 3,
      ltsMaxAgeSec = 60,
      autoEndAfterDropSec = 10 * 60,
      degradedTimeoutSec = 60 * 60,
      offRouteMeters = 300,
      offRouteSec = 120,
      boardProximityM = 150,
      boardAttemptsPer10Min = 5,
      noLocationSpotBookingsPerVehiclePerDay = 5
    }

tunablesFrom :: Maybe DRC.RiderConfig -> SharedCabTunables
tunablesFrom Nothing = defaultTunables
tunablesFrom (Just rc) =
  SharedCabTunables
    { allocationWindowSec = pick rc.sharedCabAllocationWindowSec (.allocationWindowSec),
      atStopRadiusM = pick rc.sharedCabAtStopRadiusM (.atStopRadiusM),
      walkBufferSec = pick rc.sharedCabWalkBufferSec (.walkBufferSec),
      standTimerSec = pick rc.sharedCabStandTimerSec (.standTimerSec),
      movingTimerSec = pick rc.sharedCabMovingTimerSec (.movingTimerSec),
      maxAttempts = pick rc.sharedCabMaxAttempts (.maxAttempts),
      fallbackAfterSec = pick rc.sharedCabFallbackAfterSec (.fallbackAfterSec),
      noCabGraceSec = pick rc.sharedCabNoCabGraceSec (.noCabGraceSec),
      findingTimeoutSec = pick rc.sharedCabFindingTimeoutSec (.findingTimeoutSec),
      tickSec = pick rc.sharedCabTickSec (.tickSec),
      ltsMaxAgeSec = pick rc.sharedCabLtsMaxAgeSec (.ltsMaxAgeSec),
      autoEndAfterDropSec = pick rc.sharedCabAutoEndAfterDropSec (.autoEndAfterDropSec),
      degradedTimeoutSec = pick rc.sharedCabDegradedTimeoutSec (.degradedTimeoutSec),
      offRouteMeters = pick rc.sharedCabOffRouteMeters (.offRouteMeters),
      offRouteSec = pick rc.sharedCabOffRouteSec (.offRouteSec),
      boardProximityM = pick rc.sharedCabBoardProximityM (.boardProximityM),
      boardAttemptsPer10Min = pick rc.sharedCabBoardAttemptsPer10Min (.boardAttemptsPer10Min),
      noLocationSpotBookingsPerVehiclePerDay = pick rc.sharedCabNoLocationSpotBookingsPerVehiclePerDay (.noLocationSpotBookingsPerVehiclePerDay)
    }
  where
    pick mbValue field = fromMaybe (field defaultTunables) mbValue

getTunables :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> m SharedCabTunables
getTunables cityId = tunablesFrom <$> getConfig (RiderConfigDimensions {merchantOperatingCityId = cityId.getId}) Nothing
