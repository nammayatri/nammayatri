-- | Supply-side knob dispatch for shared-route runs (a Ride with trip category SharedRoute).
--
-- Every @case@ on a supply knob lives here. The run engine, the fallback job and the overlap check ask
-- these functions instead of looking at the config, so a new product is a transporter_config change.
-- An arm no live product needs yet refuses with 'notImplemented' and names the module that owns it.
module SharedLogic.SharedRun.Knobs
  ( supplyKnobsOf,
    requireSupplyKnobs,
    notImplemented,
    DriverFeeRule (..),
    driverFee,
    runFare,
    clashWindow,
    checkInDue,
    fallbackDue,
    noShowWait,
  )
where

import Domain.Types.SharedRoute hiding (driverFee)
import qualified Domain.Types.SharedRoute as SharedRoute
import qualified Domain.Types.TransporterConfig as DTC
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common

-- | The city's supply knobs, Nothing where no runs are materialised.
supplyKnobsOf :: DTC.TransporterConfig -> Maybe SharedSupplyKnobs
supplyKnobsOf transporterConfig = transporterConfig.sharedRunConfig

-- | The city's supply knobs, validated. A city materialising runs without them, or with an impossible
-- pair, is a config error and fails here.
requireSupplyKnobs :: MonadFlow m => DTC.TransporterConfig -> m SharedSupplyKnobs
requireSupplyKnobs transporterConfig = do
  knobs <- supplyKnobsOf transporterConfig & fromMaybeM (InvalidRequest $ "No sharedRunConfig on transporter config for city " <> transporterConfig.merchantOperatingCityId.getId)
  either (throwError . InternalError . (("Invalid sharedRunConfig for city " <> transporterConfig.merchantOperatingCityId.getId <> ": ") <>)) pure (validateSupplyKnobs knobs)
  pure knobs

-- | A knob value that exists in the model but that no product has needed yet. Say which module owns it.
notImplemented :: MonadFlow m => Text -> m a
notImplemented what = throwError $ InternalError $ "shared run: " <> what <> " is not implemented yet"

-- | How the platform's take on a run is worked out at run end.
data DriverFeeRule
  = -- | the fare policy's commission config on the run fare, as for any ride (calculateCommission)
    CommissionFromFarePolicy
  | -- | a flat amount per run
    FlatCommissionPerRun
  | -- | nothing per run: a subscription plan or a salaried fleet covers it
    NoCommission
  deriving (Show, Eq)

driverFee :: MonadFlow m => SharedSupplyKnobs -> m DriverFeeRule
driverFee knobs = case SharedRoute.driverFee knobs of
  PER_TICKET -> pure CommissionFromFarePolicy
  PER_RUN -> notImplemented "PER_RUN fee: a flat commission from the SharedRoute fare_product row (module R)"
  PLAN_ONLY -> pure NoCommission
  NO_FEE -> pure NoCommission

-- | A run's fare is the money its tickets brought in; the fee basis only decides what is taken from it.
runFare :: [HighPrecMoney] -> HighPrecMoney
runFare = sum

-- | Gap two runs of one driver must keep, on top of the travel time between them (OverlapCheck's buffer).
clashWindow :: SharedSupplyKnobs -> NominalDiffTime
clashWindow knobs = minutes knobs.clashWindowMinutes

-- | When the assigned driver is asked to acknowledge a run leaving at @start@; Nothing when the city never asks.
checkInDue :: SharedSupplyKnobs -> UTCTime -> Maybe UTCTime
checkInDue knobs start
  | knobs.checkInOffsetMinutes <= 0 = Nothing
  | otherwise = Just $ addUTCTime (negate (minutes knobs.checkInOffsetMinutes)) start

-- | When an unacknowledged run leaving at @start@ goes to the fallback job.
fallbackDue :: SharedSupplyKnobs -> UTCTime -> UTCTime
fallbackDue knobs start = addUTCTime (negate (minutes knobs.fallbackWindowMinutes)) start

-- | How long the driver waits at a stop for a booked rider before marking a no-show.
noShowWait :: SharedSupplyKnobs -> Seconds
noShowWait knobs = Seconds knobs.noShowWaitSeconds

minutes :: Int -> NominalDiffTime
minutes n = fromIntegral (n * 60)
