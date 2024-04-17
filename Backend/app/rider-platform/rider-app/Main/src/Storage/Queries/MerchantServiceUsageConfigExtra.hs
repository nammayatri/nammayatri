{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MerchantServiceUsageConfigExtra where

import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.MerchantServiceUsageConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MerchantServiceUsageConfig as BeamMSUC
import Storage.Queries.OrphanInstances.MerchantServiceUsageConfig

-- Extra code goes here --

updateMerchantServiceUsageConfig :: KvDbFlow m r => MerchantServiceUsageConfig -> m ()
updateMerchantServiceUsageConfig MerchantServiceUsageConfig {..} = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamMSUC.getDistances getDistances,
      Se.Set BeamMSUC.getRoutes getRoutes,
      Se.Set BeamMSUC.snapToRoad snapToRoad,
      Se.Set BeamMSUC.getPlaceName getPlaceName,
      Se.Set BeamMSUC.getPlaceDetails getPlaceDetails,
      Se.Set BeamMSUC.autoComplete autoComplete,
      Se.Set BeamMSUC.smsProvidersPriorityList smsProvidersPriorityList,
      Se.Set BeamMSUC.updatedAt now
    ]
    [Se.Is BeamMSUC.merchantOperatingCityId (Se.Eq $ getId merchantOperatingCityId)]
