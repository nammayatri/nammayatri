{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Organization (module Storage.Queries.Organization, module ReExport) where

import qualified Domain.Types.Organization
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Organization as Beam
import Storage.Queries.OrganizationExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Organization.Organization -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Organization.Organization] -> m ())
createMany = traverse_ create

deleteById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Organization.Organization -> m ())
deleteById id = do deleteWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Organization.Organization -> m (Maybe Domain.Types.Organization.Organization))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByOrganizationName :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.Organization.Organization))
findByOrganizationName organizationName = do findOneWithKV [Se.Is Beam.organizationName $ Se.Eq organizationName]

updateContactDetails ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Organization.Organization -> m ())
updateContactDetails contactName contactPhoneNumber contactRole id = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.contactName contactName,
      Se.Set Beam.contactPhoneNumberEncrypted contactPhoneNumber,
      Se.Set Beam.contactPhoneNumberHash ((contactPhoneNumber <&> (.hash))),
      Se.Set Beam.contactRole contactRole,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateOrganizationAddress :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Organization.Organization -> m ())
updateOrganizationAddress organizationAddress id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.organizationAddress organizationAddress, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Organization.Organization -> m (Maybe Domain.Types.Organization.Organization))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Organization.Organization -> m ())
updateByPrimaryKey (Domain.Types.Organization.Organization {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.contactName contactName,
      Se.Set Beam.contactPhoneNumberEncrypted contactPhoneNumber,
      Se.Set Beam.contactPhoneNumberHash ((contactPhoneNumber <&> (.hash))),
      Se.Set Beam.contactRole contactRole,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.organizationAddress organizationAddress,
      Se.Set Beam.organizationName organizationName,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
