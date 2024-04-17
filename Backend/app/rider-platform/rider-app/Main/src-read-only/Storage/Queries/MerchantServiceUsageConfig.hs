{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MerchantServiceUsageConfig (module Storage.Queries.MerchantServiceUsageConfig, module ReExport) where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantServiceUsageConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MerchantServiceUsageConfig as Beam
import Storage.Queries.MerchantServiceUsageConfigExtra as ReExport

create :: KvDbFlow m r => (Domain.Types.MerchantServiceUsageConfig.MerchantServiceUsageConfig -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.MerchantServiceUsageConfig.MerchantServiceUsageConfig] -> m ())
createMany = traverse_ create

findByMerchantOperatingCityId ::
  KvDbFlow m r =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.MerchantServiceUsageConfig.MerchantServiceUsageConfig))
findByMerchantOperatingCityId (Kernel.Types.Id.Id merchantOperatingCityId) = do findOneWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId]
