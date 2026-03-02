{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PassOrganization where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PassOrganization
import qualified Domain.Types.PassType
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PassOrganization as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PassOrganization.PassOrganization -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PassOrganization.PassOrganization] -> m ())
createMany = traverse_ create

deleteById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization -> m ())
deleteById id = do deleteWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization -> m (Maybe Domain.Types.PassOrganization.PassOrganization))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByMerchantOperatingCityIdAndPassEnum ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.PassType.PassEnum -> m ([Domain.Types.PassOrganization.PassOrganization]))
findByMerchantOperatingCityIdAndPassEnum merchantOperatingCityId passEnum = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.passEnum $ Se.Eq passEnum
        ]
    ]

findByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.PassOrganization.PassOrganization))
findByPersonId personId = do findOneWithKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

updatePassOrganization ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Domain.Types.PassType.PassEnum -> Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization -> m ())
updatePassOrganization name address passEnum id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.name name, Se.Set Beam.address address, Se.Set Beam.updatedAt _now, Se.Set Beam.passEnum passEnum] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization -> m (Maybe Domain.Types.PassOrganization.PassOrganization))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PassOrganization.PassOrganization -> m ())
updateByPrimaryKey (Domain.Types.PassOrganization.PassOrganization {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.address address,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.name name,
      Se.Set Beam.passEnum passEnum,
      Se.Set Beam.personId (Kernel.Types.Id.getId personId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.PassOrganization Domain.Types.PassOrganization.PassOrganization where
  fromTType' (Beam.PassOrganizationT {..}) = do
    pure $
      Just
        Domain.Types.PassOrganization.PassOrganization
          { address = address,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            name = name,
            passEnum = passEnum,
            personId = Kernel.Types.Id.Id personId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PassOrganization Domain.Types.PassOrganization.PassOrganization where
  toTType' (Domain.Types.PassOrganization.PassOrganization {..}) = do
    Beam.PassOrganizationT
      { Beam.address = address,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.name = name,
        Beam.passEnum = passEnum,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.updatedAt = updatedAt
      }
