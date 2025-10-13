{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PassType where

import qualified Domain.Types.PassType
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PassType as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PassType.PassType -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PassType.PassType] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.PassType.PassType -> m (Maybe Domain.Types.PassType.PassType))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PassType.PassType -> m ())
updateByPrimaryKey (Domain.Types.PassType.PassType {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.catchline catchline,
      Se.Set Beam.description description,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.name name,
      Se.Set Beam.order order,
      Se.Set Beam.passCategoryId (Kernel.Types.Id.getId passCategoryId),
      Se.Set Beam.title title,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.PassType Domain.Types.PassType.PassType where
  fromTType' (Beam.PassTypeT {..}) = do
    pure $
      Just
        Domain.Types.PassType.PassType
          { catchline = catchline,
            description = description,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            name = name,
            order = order,
            passCategoryId = Kernel.Types.Id.Id passCategoryId,
            title = title,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PassType Domain.Types.PassType.PassType where
  toTType' (Domain.Types.PassType.PassType {..}) = do
    Beam.PassTypeT
      { Beam.catchline = catchline,
        Beam.description = description,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.name = name,
        Beam.order = order,
        Beam.passCategoryId = Kernel.Types.Id.getId passCategoryId,
        Beam.title = title,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
