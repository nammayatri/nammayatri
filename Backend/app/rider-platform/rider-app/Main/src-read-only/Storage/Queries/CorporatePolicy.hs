{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CorporatePolicy (module Storage.Queries.CorporatePolicy, module ReExport) where

import qualified Domain.Types.CorporateEntity
import qualified Domain.Types.CorporatePolicy
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CorporatePolicy as Beam
import Storage.Queries.CorporatePolicyExtra as ReExport
import Storage.Queries.OrphanInstances.CorporatePolicy ()

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CorporatePolicy.CorporatePolicy -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.CorporatePolicy.CorporatePolicy] -> m ())
createMany = traverse_ create

findByCorporateEntityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.CorporateEntity.CorporateEntity -> m [Domain.Types.CorporatePolicy.CorporatePolicy])
findByCorporateEntityId corporateEntityId = do findAllWithKV [Se.Is Beam.corporateEntityId $ Se.Eq (Kernel.Types.Id.getId corporateEntityId)]

findActiveByCorporateEntityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.CorporateEntity.CorporateEntity -> Kernel.Prelude.Bool -> m [Domain.Types.CorporatePolicy.CorporatePolicy])
findActiveByCorporateEntityId corporateEntityId isActive = do
  findAllWithKV [Se.And [Se.Is Beam.corporateEntityId $ Se.Eq (Kernel.Types.Id.getId corporateEntityId), Se.Is Beam.isActive $ Se.Eq isActive]]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.CorporatePolicy.CorporatePolicy -> m (Maybe Domain.Types.CorporatePolicy.CorporatePolicy))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
