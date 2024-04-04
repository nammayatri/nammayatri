{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.ServicePeopleCategory where

import qualified Domain.Types.ServicePeopleCategory
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.ServicePeopleCategory as Beam

create :: KvDbFlow m r => (Domain.Types.ServicePeopleCategory.ServicePeopleCategory -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.ServicePeopleCategory.ServicePeopleCategory] -> m ())
createMany = traverse_ create

findById :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.ServicePeopleCategory.ServicePeopleCategory -> m (Maybe Domain.Types.ServicePeopleCategory.ServicePeopleCategory))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.Is Beam.id $ Se.Eq id]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.ServicePeopleCategory.ServicePeopleCategory -> m (Maybe Domain.Types.ServicePeopleCategory.ServicePeopleCategory))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.ServicePeopleCategory.ServicePeopleCategory -> m ())
updateByPrimaryKey (Domain.Types.ServicePeopleCategory.ServicePeopleCategory {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.description description,
      Se.Set Beam.name name,
      Se.Set Beam.currency ((Kernel.Prelude.Just . (.currency)) pricePerUnit),
      Se.Set Beam.pricePerUnit ((.amount) pricePerUnit),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.ServicePeopleCategory Domain.Types.ServicePeopleCategory.ServicePeopleCategory where
  fromTType' (Beam.ServicePeopleCategoryT {..}) = do
    pure $
      Just
        Domain.Types.ServicePeopleCategory.ServicePeopleCategory
          { description = description,
            id = Kernel.Types.Id.Id id,
            name = name,
            pricePerUnit = Kernel.Types.Common.mkPrice currency pricePerUnit,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.ServicePeopleCategory Domain.Types.ServicePeopleCategory.ServicePeopleCategory where
  toTType' (Domain.Types.ServicePeopleCategory.ServicePeopleCategory {..}) = do
    Beam.ServicePeopleCategoryT
      { Beam.description = description,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.name = name,
        Beam.currency = (Kernel.Prelude.Just . (.currency)) pricePerUnit,
        Beam.pricePerUnit = (.amount) pricePerUnit,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
