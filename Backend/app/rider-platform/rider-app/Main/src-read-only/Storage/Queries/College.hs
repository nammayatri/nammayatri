{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.College (module Storage.Queries.College, module ReExport) where

import qualified Domain.Types.College
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.College as Beam
import Storage.Queries.CollegeExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.College.College -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.College.College] -> m ())
createMany = traverse_ create

deleteById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.College.College -> m ())
deleteById id = do deleteWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByCollegeName :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.College.College))
findByCollegeName collegeName = do findOneWithKV [Se.Is Beam.collegeName $ Se.Eq collegeName]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.College.College -> m (Maybe Domain.Types.College.College))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateCollegeAddress :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.College.College -> m ())
updateCollegeAddress collegeAddress id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.collegeAddress collegeAddress, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateContactDetails ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.College.College -> m ())
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

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.College.College -> m (Maybe Domain.Types.College.College))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.College.College -> m ())
updateByPrimaryKey (Domain.Types.College.College {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.collegeAddress collegeAddress,
      Se.Set Beam.collegeName collegeName,
      Se.Set Beam.contactName contactName,
      Se.Set Beam.contactPhoneNumberEncrypted contactPhoneNumber,
      Se.Set Beam.contactPhoneNumberHash ((contactPhoneNumber <&> (.hash))),
      Se.Set Beam.contactRole contactRole,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
