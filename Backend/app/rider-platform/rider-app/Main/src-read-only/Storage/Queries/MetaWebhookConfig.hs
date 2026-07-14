{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MetaWebhookConfig (module Storage.Queries.MetaWebhookConfig, module ReExport) where

import qualified Data.Aeson
import qualified Domain.Types.MetaWebhookConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MetaWebhookConfig as Beam
import Storage.Queries.MetaWebhookConfigExtra as ReExport
import Storage.Queries.Transformers.MetaWebhookConfig

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MetaWebhookConfig.MetaWebhookConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MetaWebhookConfig.MetaWebhookConfig] -> m ())
createMany = traverse_ create

findByPhoneNumberId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.MetaWebhookConfig.MetaWebhookConfig))
findByPhoneNumberId phoneNumberId = do findOneWithKV [Se.Is Beam.phoneNumberId $ Se.Eq phoneNumberId]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MetaWebhookConfig.MetaWebhookConfig -> m (Maybe Domain.Types.MetaWebhookConfig.MetaWebhookConfig))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MetaWebhookConfig.MetaWebhookConfig -> m ())
updateByPrimaryKey (Domain.Types.MetaWebhookConfig.MetaWebhookConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.accessToken (accessToken & unEncrypted),
      Se.Set Beam.apiVersion apiVersion,
      Se.Set Beam.appSecret (appSecret & unEncrypted),
      Se.Set Beam.baseUrl baseUrl,
      Se.Set Beam.botConfig (Data.Aeson.toJSON botConfig),
      Se.Set Beam.enabled enabled,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.phoneNumberId phoneNumberId,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.verifyToken (verifyToken & unEncrypted)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
