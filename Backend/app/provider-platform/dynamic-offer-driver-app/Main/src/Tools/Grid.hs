module Tools.Grid (getGridCfg) where

import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantServiceConfig as DOSC
import qualified Kernel.External.Grid.Types as Grid
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))

-- | The city's Grid credentials (merchant_service_config row "Grid_Grid"). Nothing when Grid is not set up for the city.
getGridCfg :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Merchant -> Id MerchantOperatingCity -> m (Maybe Grid.GridCfg)
getGridCfg merchantId merchantOpCityId = do
  mbCfg <- getOneConfig (MerchantServiceConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, merchantId = Just merchantId.getId, serviceName = Just (DOSC.GridService DOSC.Grid)}) Nothing
  pure $
    mbCfg >>= \c -> case c.serviceConfig of
      DOSC.GridServiceConfig cfg -> Just cfg
      _ -> Nothing
