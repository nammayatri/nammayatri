{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.ReferalPayout where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.ReferalPayout
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.ReferalPayout as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ReferalPayout.ReferalPayout -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.ReferalPayout.ReferalPayout] -> m ())
createMany = traverse_ create

findAllByMerchantOpCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ([Domain.Types.ReferalPayout.ReferalPayout]))
findAllByMerchantOpCityId (Kernel.Types.Id.Id merchantOperatingCityId) = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.ReferalPayout.ReferalPayout -> m (Maybe Domain.Types.ReferalPayout.ReferalPayout))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ReferalPayout.ReferalPayout -> m ())
updateByPrimaryKey (Domain.Types.ReferalPayout.ReferalPayout {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.ReferalPayout Domain.Types.ReferalPayout.ReferalPayout where
  fromTType' (Beam.ReferalPayoutT {..}) = do
    pure $
      Just
        Domain.Types.ReferalPayout.ReferalPayout
          { id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.ReferalPayout Domain.Types.ReferalPayout.ReferalPayout where
  toTType' (Domain.Types.ReferalPayout.ReferalPayout {..}) = do
    Beam.ReferalPayoutT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
