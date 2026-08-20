{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.IntercityPlatformFeeSlab where

import qualified Domain.Types.IntercityPlatformFeeSlab
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.IntercityPlatformFeeSlab as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.IntercityPlatformFeeSlab.IntercityPlatformFeeSlab -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.IntercityPlatformFeeSlab.IntercityPlatformFeeSlab] -> m ())
createMany = traverse_ create

findAllByMerchantOpCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> m [Domain.Types.IntercityPlatformFeeSlab.IntercityPlatformFeeSlab])
findAllByMerchantOpCityId merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Int -> m (Maybe Domain.Types.IntercityPlatformFeeSlab.IntercityPlatformFeeSlab))
findByPrimaryKey merchantOperatingCityId minDistanceMeters = do findOneWithKV [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId, Se.Is Beam.minDistanceMeters $ Se.Eq minDistanceMeters]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.IntercityPlatformFeeSlab.IntercityPlatformFeeSlab -> m ())
updateByPrimaryKey (Domain.Types.IntercityPlatformFeeSlab.IntercityPlatformFeeSlab {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.maxDistanceMeters maxDistanceMeters,
      Se.Set Beam.platformFee platformFee,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId, Se.Is Beam.minDistanceMeters $ Se.Eq minDistanceMeters]]

instance FromTType' Beam.IntercityPlatformFeeSlab Domain.Types.IntercityPlatformFeeSlab.IntercityPlatformFeeSlab where
  fromTType' (Beam.IntercityPlatformFeeSlabT {..}) = do
    pure $
      Just
        Domain.Types.IntercityPlatformFeeSlab.IntercityPlatformFeeSlab
          { maxDistanceMeters = maxDistanceMeters,
            merchantOperatingCityId = merchantOperatingCityId,
            minDistanceMeters = minDistanceMeters,
            platformFee = platformFee,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.IntercityPlatformFeeSlab Domain.Types.IntercityPlatformFeeSlab.IntercityPlatformFeeSlab where
  toTType' (Domain.Types.IntercityPlatformFeeSlab.IntercityPlatformFeeSlab {..}) = do
    Beam.IntercityPlatformFeeSlabT
      { Beam.maxDistanceMeters = maxDistanceMeters,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.minDistanceMeters = minDistanceMeters,
        Beam.platformFee = platformFee,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
