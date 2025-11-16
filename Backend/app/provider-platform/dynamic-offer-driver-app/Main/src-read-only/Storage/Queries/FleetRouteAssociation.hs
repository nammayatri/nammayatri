{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetRouteAssociation where

import qualified Domain.Types.FleetRouteAssociation
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetRouteAssociation as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetRouteAssociation.FleetRouteAssociation -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FleetRouteAssociation.FleetRouteAssociation] -> m ())
createMany = traverse_ create

findAllByFleetOwnerIdAndCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.FleetRouteAssociation.FleetRouteAssociation])
findAllByFleetOwnerIdAndCityId fleetOwnerId merchantOperatingCityId = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.fleetOwnerId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerId),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FleetRouteAssociation.FleetRouteAssociation -> m (Maybe Domain.Types.FleetRouteAssociation.FleetRouteAssociation))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetRouteAssociation.FleetRouteAssociation -> m ())
updateByPrimaryKey (Domain.Types.FleetRouteAssociation.FleetRouteAssociation {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.fleetOwnerId (Kernel.Types.Id.getId fleetOwnerId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.routeCode routeCode,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FleetRouteAssociation Domain.Types.FleetRouteAssociation.FleetRouteAssociation where
  fromTType' (Beam.FleetRouteAssociationT {..}) = do
    pure $
      Just
        Domain.Types.FleetRouteAssociation.FleetRouteAssociation
          { createdAt = createdAt,
            fleetOwnerId = Kernel.Types.Id.Id fleetOwnerId,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            routeCode = routeCode,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FleetRouteAssociation Domain.Types.FleetRouteAssociation.FleetRouteAssociation where
  toTType' (Domain.Types.FleetRouteAssociation.FleetRouteAssociation {..}) = do
    Beam.FleetRouteAssociationT
      { Beam.createdAt = createdAt,
        Beam.fleetOwnerId = Kernel.Types.Id.getId fleetOwnerId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.routeCode = routeCode,
        Beam.updatedAt = updatedAt
      }
