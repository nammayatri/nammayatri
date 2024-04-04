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
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MerchantConfigs as Beam

create :: KvDbFlow m r => (Domain.Types.MerchantConfigs.MerchantConfigs -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.MerchantConfigs.MerchantConfigs] -> m ())
createMany = traverse_ create

findByMerchantId :: KvDbFlow m r => (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> m (Maybe Domain.Types.MerchantConfigs.MerchantConfigs))
findByMerchantId merchantId = do findOneWithKV [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId)]

findByRequestWebHook :: KvDbFlow m r => (Kernel.Prelude.Bool -> m ([Domain.Types.MerchantConfigs.MerchantConfigs]))
findByRequestWebHook requestWebHook = do findAllWithKV [Se.Is Beam.requestWebHook $ Se.Eq requestWebHook]

updateRequestWebHookById :: KvDbFlow m r => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.MerchantConfigs.MerchantConfigs -> m ())
updateRequestWebHookById requestWebHook (Kernel.Types.Id.Id id) = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.requestWebHook requestWebHook, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq id]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.MerchantConfigs.MerchantConfigs -> m (Maybe Domain.Types.MerchantConfigs.MerchantConfigs))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.MerchantConfigs.MerchantConfigs -> m ())
updateByPrimaryKey (Domain.Types.MerchantConfigs.MerchantConfigs {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.merchantShortId merchantShortId,
      Se.Set Beam.requestWebHook requestWebHook,
      Se.Set Beam.webHookHeaders webHookHeaders,
      Se.Set Beam.webHookUrl webHookUrl,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.createdAt createdAt,
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
