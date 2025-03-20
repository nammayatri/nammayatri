{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetCSV where

import qualified Data.Time.Calendar
import qualified Domain.Types.FleetCSV
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetCSV as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetCSV.FleetCSV -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FleetCSV.FleetCSV] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Data.Time.Calendar.Day -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.FleetCSV.FleetCSV))
findByPrimaryKey day fleetOwnerId = do findOneWithKV [Se.And [Se.Is Beam.day $ Se.Eq day, Se.Is Beam.fleetOwnerId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetCSV.FleetCSV -> m ())
updateByPrimaryKey (Domain.Types.FleetCSV.FleetCSV {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.createdAt createdAt,
      Se.Set Beam.filePath filePath,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.day $ Se.Eq day, Se.Is Beam.fleetOwnerId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerId)]]

instance FromTType' Beam.FleetCSV Domain.Types.FleetCSV.FleetCSV where
  fromTType' (Beam.FleetCSVT {..}) = do
    pure $
      Just
        Domain.Types.FleetCSV.FleetCSV
          { createdAt = createdAt,
            day = day,
            filePath = filePath,
            fleetOwnerId = Kernel.Types.Id.Id fleetOwnerId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FleetCSV Domain.Types.FleetCSV.FleetCSV where
  toTType' (Domain.Types.FleetCSV.FleetCSV {..}) = do
    Beam.FleetCSVT
      { Beam.createdAt = createdAt,
        Beam.day = day,
        Beam.filePath = filePath,
        Beam.fleetOwnerId = Kernel.Types.Id.getId fleetOwnerId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.updatedAt = updatedAt
      }
