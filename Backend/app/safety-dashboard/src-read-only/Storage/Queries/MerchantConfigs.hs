{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MerchantConfigs where

import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantConfigs
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MerchantConfigs as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantConfigs.MerchantConfigs -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MerchantConfigs.MerchantConfigs] -> m ())
createMany = traverse_ create

findByMerchantId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> m (Maybe Domain.Types.MerchantConfigs.MerchantConfigs))
findByMerchantId merchantId = do findOneWithKV [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId)]

findByRequestWebHook :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> m [Domain.Types.MerchantConfigs.MerchantConfigs])
findByRequestWebHook requestWebHook = do findAllWithKV [Se.Is Beam.requestWebHook $ Se.Eq requestWebHook]

updateRequestWebHookById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.MerchantConfigs.MerchantConfigs -> m ())
updateRequestWebHookById requestWebHook id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.requestWebHook requestWebHook, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MerchantConfigs.MerchantConfigs -> m (Maybe Domain.Types.MerchantConfigs.MerchantConfigs))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantConfigs.MerchantConfigs -> m ())
updateByPrimaryKey (Domain.Types.MerchantConfigs.MerchantConfigs {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.merchantShortId merchantShortId,
      Se.Set Beam.requestWebHook requestWebHook,
      Se.Set Beam.webHookHeaders webHookHeaders,
      Se.Set Beam.webHookUrl webHookUrl,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.MerchantConfigs Domain.Types.MerchantConfigs.MerchantConfigs where
  fromTType' (Beam.MerchantConfigsT {..}) = do
    pure $
      Just
        Domain.Types.MerchantConfigs.MerchantConfigs
          { id = Kernel.Types.Id.Id id,
            merchantShortId = merchantShortId,
            requestWebHook = requestWebHook,
            webHookHeaders = webHookHeaders,
            webHookUrl = webHookUrl,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.MerchantConfigs Domain.Types.MerchantConfigs.MerchantConfigs where
  toTType' (Domain.Types.MerchantConfigs.MerchantConfigs {..}) = do
    Beam.MerchantConfigsT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantShortId = merchantShortId,
        Beam.requestWebHook = requestWebHook,
        Beam.webHookHeaders = webHookHeaders,
        Beam.webHookUrl = webHookUrl,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
