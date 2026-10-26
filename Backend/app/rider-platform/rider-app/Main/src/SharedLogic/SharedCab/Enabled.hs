-- | The per-city shared-cab switch: on iff the city has a SHARED_CAB IntegratedBPPConfig and ops have not
-- turned it off with rider_config.shared_cab_enabled = false (NULL follows the data).
module SharedLogic.SharedCab.Enabled
  ( sharedCabEnabled,
    sharedCabEnabledDecision,
    requireSharedCabEnabled,
    tickChainContinues,
    cabItineraryShown,
  )
where

import qualified BecknV2.OnDemand.Enums as Enums
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import SharedLogic.SharedCab.LegState (isSharedCabAgency)
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import Tools.Error (GenericError (InvalidRequest))

-- | A suspended city takes no new intake (sessions, bookings, boardings, walk-ups, search results); what is already in flight
-- still drains: finding timeouts, refunds, session expiry and degraded-ride expiry keep running.
requireSharedCabEnabled :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> m ()
requireSharedCabEnabled cityId = sharedCabEnabled cityId >>= \enabled -> unless enabled $ throwError (InvalidRequest "Shared cab is suspended in your city")

-- | The allocation chain lives while the city is on, or while live bookings still need draining.
tickChainContinues :: Bool -> Int -> Bool
tickChainContinues enabled liveBookings = enabled || liveBookings > 0

-- | A cab itinerary is offered only in an enabled city and only while a cab runs every cab leg's route.
cabItineraryShown :: Bool -> [Bool] -> Bool
cabItineraryShown enabled cabsComing = null cabsComing || (enabled && and cabsComing)

sharedCabEnabledDecision :: Bool -> Maybe Bool -> Bool
sharedCabEnabledDecision hasSharedCabConfig opsSwitch = hasSharedCabConfig && opsSwitch /= Just False

sharedCabEnabled :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> m Bool
sharedCabEnabled cityId = do
  configs <- SIBC.findAllIntegratedBPPConfig cityId Enums.BUS DIBC.APPLICATION
  riderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = cityId.getId}) Nothing
  pure $ sharedCabEnabledDecision (any (isSharedCabAgency . (.agencyKey)) configs) (riderConfig >>= (.sharedCabEnabled))
