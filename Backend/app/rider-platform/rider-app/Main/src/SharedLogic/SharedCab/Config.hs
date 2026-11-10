module SharedLogic.SharedCab.Config
  ( SharedCabTunables (..),
    defaultTunables,
    tunablesFrom,
    tunablesOf,
    getTunables,
    activeCallMode,
  )
where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.RiderConfig as DRC
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))

-- | The tunables, all in seconds or metres. Each can be set in the city's rider_config.sharedCabConfig JSON under the same name;
-- a missing field (or no row) takes the default.
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
    callMode :: DRC.SharedCabCallMode,
    chatEnabled :: Bool,
    -- | The rider may pick a specific cab; off unless the city's rider_config turns it on.
    pickEnabled :: Bool,
    -- | How long a rider who opened a stand's cab view still counts as waiting for a cab on its routes.
    interestWindowSec :: Int,
    -- | How many stops from the start of each route count towards its demand.
    demandStopsPerRoute :: Int,
    -- | The most routes one demand read may ask for.
    maxDemandRoutes :: Int,
    -- | A claim is an offer the driver accepts or declines; off unless the city's rider_config turns it on.
    driverAcceptEnabled :: Bool,
    -- | Seconds the driver has to answer the first offer to a booking, and each later one (after a decline or a timeout).
    offerTimeoutSec :: Int,
    offerRetryTimeoutSec :: Int,
    -- | Rider skips, and separately rider cancels, allowed per search while its counts last (skipCancelCountTtlSec).
    maxSkipsAndCancels :: Int,
    skipCancelCountTtlSec :: Int,
    -- | The assigned cab's ETA to the board stop must be over this for the rider to skip it.
    skipMinEtaSec :: Int
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
      offRouteSec = 120,
      boardProximityM = 150,
      boardPingMaxAgeSec = 10,
      boardAttemptsPer10Min = 5,
      noLocationSpotBookingsPerVehiclePerDay = 5,
      maxCabSpeedMps = 14,
      callEnabled = False,
      callMode = DRC.MASKED,
      chatEnabled = False,
      pickEnabled = False,
      interestWindowSec = 10 * 60,
      demandStopsPerRoute = 5,
      maxDemandRoutes = 50,
      driverAcceptEnabled = False,
      offerTimeoutSec = 30,
      offerRetryTimeoutSec = 15,
      maxSkipsAndCancels = 15,
      skipCancelCountTtlSec = 5 * 60 * 60,
      skipMinEtaSec = 120
    }

tunablesFrom :: Maybe DRC.RiderConfig -> SharedCabTunables
tunablesFrom mbRc = tunablesOf (mbRc >>= (.boardingBusPingMaxAgeSeconds)) (mbRc >>= (.sharedCabConfig))

-- | The city's sharedCabConfig over the defaults; the board ping bar reuses the bus boarding's rider_config column.
tunablesOf :: Maybe Double -> Maybe DRC.SharedCabConfig -> SharedCabTunables
tunablesOf busPingMaxAgeSec mbConfig =
  SharedCabTunables
    { allocationWindowSec = pick (.allocationWindowSec) (.allocationWindowSec),
      atStopRadiusM = pick (.atStopRadiusM) (.atStopRadiusM),
      standTimerSec = pick (.standTimerSec) (.standTimerSec),
      movingTimerSec = pick (.movingTimerSec) (.movingTimerSec),
      maxAttempts = pick (.maxAttempts) (.maxAttempts),
      maxNoShows = pick (.maxNoShows) (.maxNoShows),
      fallbackAfterSec = pick (.fallbackAfterSec) (.fallbackAfterSec),
      findingTimeoutSec = pick (.findingTimeoutSec) (.findingTimeoutSec),
      tickSec = pick (.tickSec) (.tickSec),
      ltsMaxAgeSec = pick (.ltsMaxAgeSec) (.ltsMaxAgeSec),
      autoEndAfterDropSec = pick (.autoEndAfterDropSec) (.autoEndAfterDropSec),
      degradedTimeoutSec = pick (.degradedTimeoutSec) (.degradedTimeoutSec),
      offRouteSec = pick (.offRouteSec) (.offRouteSec),
      boardProximityM = pick (.boardProximityM) (.boardProximityM),
      boardPingMaxAgeSec = fromMaybe defaultTunables.boardPingMaxAgeSec busPingMaxAgeSec,
      boardAttemptsPer10Min = pick (.boardAttemptsPer10Min) (.boardAttemptsPer10Min),
      noLocationSpotBookingsPerVehiclePerDay = pick (.noLocationSpotBookingsPerVehiclePerDay) (.noLocationSpotBookingsPerVehiclePerDay),
      maxCabSpeedMps = pick (.maxCabSpeedMps) (.maxCabSpeedMps),
      callEnabled = pick (.callEnabled) (.callEnabled),
      callMode = pick (.callMode) (.callMode),
      chatEnabled = pick (.chatEnabled) (.chatEnabled),
      pickEnabled = pick (.pickEnabled) (.pickEnabled),
      interestWindowSec = pick (.interestWindowSec) (.interestWindowSec),
      demandStopsPerRoute = pickPositive (.demandStopsPerRoute) (.demandStopsPerRoute),
      maxDemandRoutes = pickPositive (.maxDemandRoutes) (.maxDemandRoutes),
      driverAcceptEnabled = pick (.driverAcceptEnabled) (.driverAcceptEnabled),
      offerTimeoutSec = pick (.offerTimeoutSec) (.offerTimeoutSec),
      offerRetryTimeoutSec = pick (.offerRetryTimeoutSec) (.offerRetryTimeoutSec),
      maxSkipsAndCancels = pick (.maxSkipsAndCancels) (.maxSkipsAndCancels),
      skipCancelCountTtlSec = pickPositive (.skipCancelCountTtlSec) (.skipCancelCountTtlSec),
      skipMinEtaSec = pick (.skipMinEtaSec) (.skipMinEtaSec)
    }
  where
    pick :: (DRC.SharedCabConfig -> Maybe a) -> (SharedCabTunables -> a) -> a
    pick get field = fromMaybe (field defaultTunables) (mbConfig >>= get)
    pickPositive :: (DRC.SharedCabConfig -> Maybe Int) -> (SharedCabTunables -> Int) -> Int
    pickPositive get = pick (mfilter (> 0) . get)

getTunables :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> m SharedCabTunables
getTunables cityId = tunablesFrom <$> getConfig (RiderConfigDimensions {merchantOperatingCityId = cityId.getId}) Nothing

-- | How the rider and the cab's driver reach each other, Nothing while the city's calls are off: MASKED through the city's ExoPhone,
-- DIRECT on each other's own number. rider-app decides for both sides, so driver-app needs no setting of its own.
activeCallMode :: SharedCabTunables -> Maybe DRC.SharedCabCallMode
activeCallMode cfg = if cfg.callEnabled then Just cfg.callMode else Nothing
