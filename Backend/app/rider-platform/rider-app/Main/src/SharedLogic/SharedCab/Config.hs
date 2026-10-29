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

-- | The tunables, all in seconds or metres. Each has a nullable rider_config column
-- `sharedCab<Field>`; NULL (or no row) takes the default.
data SharedCabTunables = SharedCabTunables
  { allocationWindowSec :: Int,
    atStopRadiusM :: Int,
    standTimerSec :: Int,
    movingTimerSec :: Int,
    maxAttempts :: Int,
    maxNoShows :: Int,
    fallbackAfterSec :: Int,
    findingTimeoutSec :: Int,
    tickSec :: Int,
    ltsMaxAgeSec :: Int,
    autoEndAfterDropSec :: Int,
    degradedTimeoutSec :: Int,
    offRouteMeters :: Int,
    offRouteSec :: Int,
    boardProximityM :: Int,
    -- | The bus boarding's freshness bar (rider_config.boardingBusPingMaxAgeSeconds), for the cab's ping and the rider's point.
    boardPingMaxAgeSec :: Double,
    boardAttemptsPer10Min :: Int,
    noLocationSpotBookingsPerVehiclePerDay :: Int,
    -- | Fastest a cab can plausibly travel (m/s): a candidate whose straight-line distance to the stop exceeds its ETA times this (plus
    -- the at-stop radius) has a bad ETA.
    maxCabSpeedMps :: Int,
    -- | Masked rider/driver call and per-booking chat; off unless the city's rider_config turns them on.
    callEnabled :: Bool,
    chatEnabled :: Bool,
    -- | The rider may pick a specific cab; off unless the city's rider_config turns it on.
    pickEnabled :: Bool,
    -- | How long a rider who opened a stand's cab view still counts as waiting for a cab on its routes.
    interestWindowSec :: Int,
    -- | A claim is an offer the driver accepts or declines; off unless the city's rider_config turns it on.
    driverAcceptEnabled :: Bool,
    -- | Seconds the driver has to answer the first offer to a booking, and each later one (after a decline or a timeout).
    offerTimeoutSec :: Int,
    offerRetryTimeoutSec :: Int
  }
  deriving (Show, Eq, Generic)

defaultTunables :: SharedCabTunables
defaultTunables =
  SharedCabTunables
    { allocationWindowSec = 8 * 60,
      atStopRadiusM = 100,
      standTimerSec = 180,
      movingTimerSec = 90,
      maxAttempts = 2,
      maxNoShows = 2,
      fallbackAfterSec = 10 * 60,
      findingTimeoutSec = 20 * 60,
      tickSec = 3,
      ltsMaxAgeSec = 60,
      autoEndAfterDropSec = 10 * 60,
      degradedTimeoutSec = 60 * 60,
      offRouteMeters = 300,
      offRouteSec = 120,
      boardProximityM = 150,
      boardPingMaxAgeSec = 10,
      boardAttemptsPer10Min = 5,
      noLocationSpotBookingsPerVehiclePerDay = 5,
      maxCabSpeedMps = 14,
      callEnabled = False,
      chatEnabled = False,
      pickEnabled = False,
      interestWindowSec = 10 * 60,
      driverAcceptEnabled = False,
      offerTimeoutSec = 30,
      offerRetryTimeoutSec = 15
    }

tunablesFrom :: Maybe DRC.RiderConfig -> SharedCabTunables
tunablesFrom Nothing = defaultTunables
tunablesFrom (Just rc) =
  SharedCabTunables
    { allocationWindowSec = pick rc.sharedCabAllocationWindowSec (.allocationWindowSec),
      atStopRadiusM = pick rc.sharedCabAtStopRadiusM (.atStopRadiusM),
      standTimerSec = pick rc.sharedCabStandTimerSec (.standTimerSec),
      movingTimerSec = pick rc.sharedCabMovingTimerSec (.movingTimerSec),
      maxAttempts = pick rc.sharedCabMaxAttempts (.maxAttempts),
      maxNoShows = pick rc.sharedCabMaxNoShows (.maxNoShows),
      fallbackAfterSec = pick rc.sharedCabFallbackAfterSec (.fallbackAfterSec),
      findingTimeoutSec = pick rc.sharedCabFindingTimeoutSec (.findingTimeoutSec),
      tickSec = pick rc.sharedCabTickSec (.tickSec),
      ltsMaxAgeSec = pick rc.sharedCabLtsMaxAgeSec (.ltsMaxAgeSec),
      autoEndAfterDropSec = pick rc.sharedCabAutoEndAfterDropSec (.autoEndAfterDropSec),
      degradedTimeoutSec = pick rc.sharedCabDegradedTimeoutSec (.degradedTimeoutSec),
      offRouteMeters = pick rc.sharedCabOffRouteMeters (.offRouteMeters),
      offRouteSec = pick rc.sharedCabOffRouteSec (.offRouteSec),
      boardProximityM = pick rc.sharedCabBoardProximityM (.boardProximityM),
      boardPingMaxAgeSec = pick rc.boardingBusPingMaxAgeSeconds (.boardPingMaxAgeSec),
      boardAttemptsPer10Min = pick rc.sharedCabBoardAttemptsPer10Min (.boardAttemptsPer10Min),
      noLocationSpotBookingsPerVehiclePerDay = pick rc.sharedCabNoLocationSpotBookingsPerVehiclePerDay (.noLocationSpotBookingsPerVehiclePerDay),
      maxCabSpeedMps = pick rc.sharedCabMaxCabSpeedMps (.maxCabSpeedMps),
      callEnabled = pick rc.sharedCabCallEnabled (.callEnabled),
      chatEnabled = pick rc.sharedCabChatEnabled (.chatEnabled),
      pickEnabled = pick rc.sharedCabPickEnabled (.pickEnabled),
      interestWindowSec = pick rc.sharedCabInterestWindowSec (.interestWindowSec),
      driverAcceptEnabled = pick rc.sharedCabDriverAcceptEnabled (.driverAcceptEnabled),
      offerTimeoutSec = pick rc.sharedCabOfferTimeoutSec (.offerTimeoutSec),
      offerRetryTimeoutSec = pick rc.sharedCabOfferRetryTimeoutSec (.offerRetryTimeoutSec)
    }
  where
    pick mbValue field = fromMaybe (field defaultTunables) mbValue

getTunables :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> m SharedCabTunables
getTunables cityId = tunablesFrom <$> getConfig (RiderConfigDimensions {merchantOperatingCityId = cityId.getId}) Nothing
