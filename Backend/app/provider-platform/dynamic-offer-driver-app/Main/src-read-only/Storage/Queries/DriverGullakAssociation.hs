{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverGullakAssociation where

import qualified Domain.Types.DriverGullakAssociation
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverGullakAssociation as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverGullakAssociation.DriverGullakAssociation -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DriverGullakAssociation.DriverGullakAssociation] -> m ())
createMany = traverse_ create

updateGullakToken :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateGullakToken gullakToken tokenExpiry driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.gullakToken gullakToken, Se.Set Beam.tokenExpiry tokenExpiry, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.DriverGullakAssociation.DriverGullakAssociation))
findByPrimaryKey driverId = do findOneWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverGullakAssociation.DriverGullakAssociation -> m ())
updateByPrimaryKey (Domain.Types.DriverGullakAssociation.DriverGullakAssociation {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.gullakToken gullakToken,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.tokenExpiry tokenExpiry,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]

instance FromTType' Beam.DriverGullakAssociation Domain.Types.DriverGullakAssociation.DriverGullakAssociation where
  fromTType' (Beam.DriverGullakAssociationT {..}) = do
    pure $
      Just
        Domain.Types.DriverGullakAssociation.DriverGullakAssociation
          { driverId = Kernel.Types.Id.Id driverId,
            gullakToken = gullakToken,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            tokenExpiry = tokenExpiry,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DriverGullakAssociation Domain.Types.DriverGullakAssociation.DriverGullakAssociation where
  toTType' (Domain.Types.DriverGullakAssociation.DriverGullakAssociation {..}) = do
    Beam.DriverGullakAssociationT
      { Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.gullakToken = gullakToken,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.tokenExpiry = tokenExpiry,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
