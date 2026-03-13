{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CorporateShift (module Storage.Queries.CorporateShift, module ReExport) where

import qualified Domain.Types.CorporateEntity
import qualified Domain.Types.CorporateShift
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CorporateShift as Beam
import Storage.Queries.CorporateShiftExtra as ReExport
import Storage.Queries.OrphanInstances.CorporateShift ()

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CorporateShift.CorporateShift -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.CorporateShift.CorporateShift] -> m ())
createMany = traverse_ create

findByCorporateEntityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.CorporateEntity.CorporateEntity -> m [Domain.Types.CorporateShift.CorporateShift])
findByCorporateEntityId corporateEntityId = do findAllWithKV [Se.Is Beam.corporateEntityId $ Se.Eq (Kernel.Types.Id.getId corporateEntityId)]

updateStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.CorporateShift.CorporateShiftStatus -> Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.CorporateShift.CorporateShift -> m ())
updateStatus status updatedAt id = do
  updateWithKV
    [ Se.Set Beam.status (Kernel.Prelude.show status),
      Se.Set Beam.updatedAt updatedAt
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.CorporateShift.CorporateShift -> m (Maybe Domain.Types.CorporateShift.CorporateShift))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
