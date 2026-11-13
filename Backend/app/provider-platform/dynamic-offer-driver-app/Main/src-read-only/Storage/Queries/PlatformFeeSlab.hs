{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PlatformFeeSlab where

import qualified Domain.Types.PlatformFeeSlab
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PlatformFeeSlab as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PlatformFeeSlab.PlatformFeeSlab -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PlatformFeeSlab.PlatformFeeSlab] -> m ())
createMany = traverse_ create

findAllBySlabSetId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m [Domain.Types.PlatformFeeSlab.PlatformFeeSlab])
findAllBySlabSetId slabSetId = do findAllWithKV [Se.Is Beam.slabSetId $ Se.Eq slabSetId]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Int -> Kernel.Prelude.Text -> m (Maybe Domain.Types.PlatformFeeSlab.PlatformFeeSlab))
findByPrimaryKey minDistanceMeters slabSetId = do findOneWithKV [Se.And [Se.Is Beam.minDistanceMeters $ Se.Eq minDistanceMeters, Se.Is Beam.slabSetId $ Se.Eq slabSetId]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PlatformFeeSlab.PlatformFeeSlab -> m ())
updateByPrimaryKey (Domain.Types.PlatformFeeSlab.PlatformFeeSlab {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.cgstPercentage cgstPercentage,
      Se.Set Beam.maxDistanceMeters maxDistanceMeters,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.platformFee platformFee,
      Se.Set Beam.sgstPercentage sgstPercentage,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.minDistanceMeters $ Se.Eq minDistanceMeters, Se.Is Beam.slabSetId $ Se.Eq slabSetId]]

instance FromTType' Beam.PlatformFeeSlab Domain.Types.PlatformFeeSlab.PlatformFeeSlab where
  fromTType' (Beam.PlatformFeeSlabT {..}) = do
    pure $
      Just
        Domain.Types.PlatformFeeSlab.PlatformFeeSlab
          { cgstPercentage = cgstPercentage,
            maxDistanceMeters = maxDistanceMeters,
            merchantOperatingCityId = merchantOperatingCityId,
            minDistanceMeters = minDistanceMeters,
            platformFee = platformFee,
            sgstPercentage = sgstPercentage,
            slabSetId = slabSetId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PlatformFeeSlab Domain.Types.PlatformFeeSlab.PlatformFeeSlab where
  toTType' (Domain.Types.PlatformFeeSlab.PlatformFeeSlab {..}) = do
    Beam.PlatformFeeSlabT
      { Beam.cgstPercentage = cgstPercentage,
        Beam.maxDistanceMeters = maxDistanceMeters,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.minDistanceMeters = minDistanceMeters,
        Beam.platformFee = platformFee,
        Beam.sgstPercentage = sgstPercentage,
        Beam.slabSetId = slabSetId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
