{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetBadge (module Storage.Queries.FleetBadge, module ReExport) where

import qualified Domain.Types.FleetBadge
import qualified Domain.Types.FleetBadgeType
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetBadge as Beam
import Storage.Queries.FleetBadgeExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetBadge.FleetBadge -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FleetBadge.FleetBadge] -> m ())
createMany = traverse_ create

findAllBadgesByNameAndBadgeTypeAndFleetOwnerIds ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  ([Kernel.Types.Id.Id Domain.Types.Person.Person] -> Kernel.Prelude.Text -> Domain.Types.FleetBadgeType.FleetBadgeType -> m [Domain.Types.FleetBadge.FleetBadge])
findAllBadgesByNameAndBadgeTypeAndFleetOwnerIds fleetOwnerId badgeName badgeType = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.fleetOwnerId $ Se.In (Kernel.Types.Id.getId <$> fleetOwnerId),
          Se.Is Beam.badgeName $ Se.Eq badgeName,
          Se.Is Beam.badgeType $ Se.Eq badgeType
        ]
    ]

findOneBadgeByNameAndBadgeTypeAndFleetOwnerId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Text -> Domain.Types.FleetBadgeType.FleetBadgeType -> m (Maybe Domain.Types.FleetBadge.FleetBadge))
findOneBadgeByNameAndBadgeTypeAndFleetOwnerId fleetOwnerId badgeName badgeType = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.fleetOwnerId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerId),
          Se.Is Beam.badgeName $ Se.Eq badgeName,
          Se.Is Beam.badgeType $ Se.Eq badgeType
        ]
    ]

updateBadgeNameAndRankById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.FleetBadge.FleetBadge -> m ())
updateBadgeNameAndRankById badgeName badgeRank id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.badgeName badgeName, Se.Set Beam.badgeRank badgeRank, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FleetBadge.FleetBadge -> m (Maybe Domain.Types.FleetBadge.FleetBadge))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetBadge.FleetBadge -> m ())
updateByPrimaryKey (Domain.Types.FleetBadge.FleetBadge {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.badgeName badgeName,
      Se.Set Beam.badgeRank badgeRank,
      Se.Set Beam.badgeType badgeType,
      Se.Set Beam.fleetOwnerId (Kernel.Types.Id.getId fleetOwnerId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.personId (Kernel.Types.Id.getId <$> personId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
