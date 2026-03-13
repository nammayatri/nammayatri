{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CorporateEmployee (module Storage.Queries.CorporateEmployee, module ReExport) where

import qualified Domain.Types.CorporateEmployee
import qualified Domain.Types.CorporateEntity
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CorporateEmployee as Beam
import Storage.Queries.CorporateEmployeeExtra as ReExport
import Storage.Queries.OrphanInstances.CorporateEmployee ()

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CorporateEmployee.CorporateEmployee -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.CorporateEmployee.CorporateEmployee] -> m ())
createMany = traverse_ create

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.CorporateEmployee.CorporateEmployee -> m (Maybe Domain.Types.CorporateEmployee.CorporateEmployee))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByCorporateEntityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.CorporateEntity.CorporateEntity -> m [Domain.Types.CorporateEmployee.CorporateEmployee])
findByCorporateEntityId corporateEntityId = do findAllWithKV [Se.Is Beam.corporateEntityId $ Se.Eq (Kernel.Types.Id.getId corporateEntityId)]

findByPersonId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> m (Maybe Domain.Types.CorporateEmployee.CorporateEmployee))
findByPersonId personId = do findOneWithKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId <$> personId)]

findByPhoneAndEntity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.CorporateEntity.CorporateEntity -> m (Maybe Domain.Types.CorporateEmployee.CorporateEmployee))
findByPhoneAndEntity phone corporateEntityId = do
  findOneWithKV [Se.And [Se.Is Beam.phone $ Se.Eq phone, Se.Is Beam.corporateEntityId $ Se.Eq (Kernel.Types.Id.getId corporateEntityId)]]

updateStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.CorporateEmployee.CorporateEmployeeStatus -> Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.CorporateEmployee.CorporateEmployee -> m ())
updateStatus status updatedAt id = do
  updateWithKV
    [ Se.Set Beam.status (Kernel.Prelude.show status),
      Se.Set Beam.updatedAt updatedAt
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.CorporateEmployee.CorporateEmployee -> m (Maybe Domain.Types.CorporateEmployee.CorporateEmployee))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
