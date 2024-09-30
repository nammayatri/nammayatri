{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MerchantServiceUsageConfig (module Storage.Queries.MerchantServiceUsageConfig, module ReExport) where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantServiceUsageConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.SMS.Types
import qualified Kernel.External.Whatsapp.Types
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MerchantServiceUsageConfig as Beam
import Storage.Queries.MerchantServiceUsageConfigExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantServiceUsageConfig.MerchantServiceUsageConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MerchantServiceUsageConfig.MerchantServiceUsageConfig] -> m ())
createMany = traverse_ create

findByMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.MerchantServiceUsageConfig.MerchantServiceUsageConfig))
findByMerchantOperatingCityId merchantOperatingCityId = do findOneWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

updateSmsProvidersPriorityList ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  ([Kernel.External.SMS.Types.SmsService] -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ())
updateSmsProvidersPriorityList smsProvidersPriorityList merchantOperatingCityId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.smsProvidersPriorityList smsProvidersPriorityList, Se.Set Beam.updatedAt _now] [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

updateWhatsappProvidersPriorityList ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  ([Kernel.External.Whatsapp.Types.WhatsappService] -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ())
updateWhatsappProvidersPriorityList whatsappProvidersPriorityList merchantOperatingCityId = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.whatsappProvidersPriorityList whatsappProvidersPriorityList,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]
