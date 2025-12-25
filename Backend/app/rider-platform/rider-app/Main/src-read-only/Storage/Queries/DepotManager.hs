{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DepotManager where

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

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DepotManager.DepotManager -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DepotManager.DepotManager] -> m ())
createMany = traverse_ create

findByDepotCode :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Depot.Depot -> m (Maybe Domain.Types.DepotManager.DepotManager))
findByDepotCode depotCode = do findOneWithKV [Se.Is Beam.depotCode $ Se.Eq (Kernel.Types.Id.getId depotCode)]

findByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.DepotManager.DepotManager))
findByPersonId personId = do findOneWithKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

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

instance FromTType' Beam.DepotManager Domain.Types.DepotManager.DepotManager where
  fromTType' (Beam.DepotManagerT {..}) = do
    pure $
      Just
        Domain.Types.DepotManager.DepotManager
          { createdAt = createdAt,
            depotCode = Kernel.Types.Id.Id depotCode,
            enabled = enabled,
            isAdmin = isAdmin,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            personId = Kernel.Types.Id.Id personId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DepotManager Domain.Types.DepotManager.DepotManager where
  toTType' (Domain.Types.DepotManager.DepotManager {..}) = do
    Beam.DepotManagerT
      { Beam.createdAt = createdAt,
        Beam.depotCode = Kernel.Types.Id.getId depotCode,
        Beam.enabled = enabled,
        Beam.isAdmin = isAdmin,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.updatedAt = updatedAt
      }
