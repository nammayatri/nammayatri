-- | The per-city shared-cab switch: on iff the city has a SHARED_CAB IntegratedBPPConfig and ops have not
-- turned it off with rider_config.shared_cab_enabled = false (NULL follows the data).
module SharedLogic.SharedCab.Enabled
  ( sharedCabEnabled,
    sharedCabEnabledDecision,
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

sharedCabEnabledDecision :: Bool -> Maybe Bool -> Bool
sharedCabEnabledDecision hasSharedCabConfig opsSwitch = hasSharedCabConfig && opsSwitch /= Just False

sharedCabEnabled :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> m Bool
sharedCabEnabled cityId = do
  configs <- SIBC.findAllIntegratedBPPConfig cityId Enums.BUS DIBC.APPLICATION
  riderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = cityId.getId}) Nothing
  pure $ sharedCabEnabledDecision (any (isSharedCabAgency . (.agencyKey)) configs) (riderConfig >>= (.sharedCabEnabled))
