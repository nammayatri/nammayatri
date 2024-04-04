{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.BusinessHour where

import qualified Domain.Types.BusinessHour
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.BusinessHour as Beam

create :: KvDbFlow m r => (Domain.Types.BusinessHour.BusinessHour -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.BusinessHour.BusinessHour] -> m ())
createMany = traverse_ create

findById :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour -> m (Maybe Domain.Types.BusinessHour.BusinessHour))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.Is Beam.id $ Se.Eq id]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour -> m (Maybe Domain.Types.BusinessHour.BusinessHour))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.BusinessHour.BusinessHour -> m ())
updateByPrimaryKey (Domain.Types.BusinessHour.BusinessHour {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.btype btype,
      Se.Set Beam.categoryId (Kernel.Types.Id.getId <$> categoryId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.BusinessHour Domain.Types.BusinessHour.BusinessHour where
  fromTType' (Beam.BusinessHourT {..}) = do
    pure $
      Just
        Domain.Types.BusinessHour.BusinessHour
          { btype = btype,
            categoryId = Kernel.Types.Id.Id <$> categoryId,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.BusinessHour Domain.Types.BusinessHour.BusinessHour where
  toTType' (Domain.Types.BusinessHour.BusinessHour {..}) = do
    Beam.BusinessHourT
      { Beam.btype = btype,
        Beam.categoryId = Kernel.Types.Id.getId <$> categoryId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
