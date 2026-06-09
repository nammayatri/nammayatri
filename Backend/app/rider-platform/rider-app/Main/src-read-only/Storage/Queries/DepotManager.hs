{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DepotManager (module Storage.Queries.DepotManager, module ReExport) where

import qualified Domain.Types.Depot
import qualified Domain.Types.DepotManager
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DepotManager as Beam
import Storage.Queries.DepotManagerExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DepotManager.DepotManager -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DepotManager.DepotManager] -> m ())
createMany = traverse_ create

findByDepotCode :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Depot.Depot -> m (Maybe Domain.Types.DepotManager.DepotManager))
findByDepotCode depotCode = do findOneWithDb [Se.Is Beam.depotCode $ Se.Eq (Kernel.Types.Id.getId depotCode)]

findByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.DepotManager.DepotManager))
findByPersonId personId = do findOneWithDb [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

findByPersonIdAndDepotCode ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Depot.Depot -> m (Maybe Domain.Types.DepotManager.DepotManager))
findByPersonIdAndDepotCode personId depotCode = do findOneWithDb [Se.And [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId), Se.Is Beam.depotCode $ Se.Eq (Kernel.Types.Id.getId depotCode)]]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Depot.Depot -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.DepotManager.DepotManager))
findByPrimaryKey depotCode personId = do findOneWithKV [Se.And [Se.Is Beam.depotCode $ Se.Eq (Kernel.Types.Id.getId depotCode), Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DepotManager.DepotManager -> m ())
updateByPrimaryKey (Domain.Types.DepotManager.DepotManager {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.enabled enabled,
      Se.Set Beam.isAdmin isAdmin,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.depotCode $ Se.Eq (Kernel.Types.Id.getId depotCode), Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]]
