{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.FleetBadge (module Storage.Queries.FleetBadge, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.FleetBadgeExtra as ReExport
import qualified Domain.Types.FleetBadge
import qualified Storage.Beam.FleetBadge as Beam
import qualified Kernel.Prelude
import qualified Domain.Types.FleetBadgeType
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetBadge.FleetBadge -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FleetBadge.FleetBadge] -> m ())
createMany = traverse_ create
findAllBadgesByNameAndBadgeTypeAndFleetOwnerIds :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                                                   ([Kernel.Types.Id.Id Domain.Types.Person.Person] -> Kernel.Prelude.Text -> Domain.Types.FleetBadgeType.FleetBadgeType -> m ([Domain.Types.FleetBadge.FleetBadge]))
findAllBadgesByNameAndBadgeTypeAndFleetOwnerIds fleetOwnerId badgeName badgeType = do findAllWithKV [Se.And [Se.Is Beam.fleetOwnerId $ Se.In (Kernel.Types.Id.getId <$> fleetOwnerId),
                                                                                                             Se.Is Beam.badgeName $ Se.Eq badgeName,
                                                                                                             Se.Is Beam.badgeType $ Se.Eq badgeType]]
findOneBadgeByNameAndBadgeTypeAndFleetOwnerId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                                                 (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Text -> Domain.Types.FleetBadgeType.FleetBadgeType -> m (Maybe Domain.Types.FleetBadge.FleetBadge))
findOneBadgeByNameAndBadgeTypeAndFleetOwnerId fleetOwnerId badgeName badgeType = do findOneWithKV [Se.And [Se.Is Beam.fleetOwnerId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerId),
                                                                                                           Se.Is Beam.badgeName $ Se.Eq badgeName,
                                                                                                           Se.Is Beam.badgeType $ Se.Eq badgeType]]
updateBadgeNameAndRankById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                              (Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.FleetBadge.FleetBadge -> m ())
updateBadgeNameAndRankById badgeName badgeRank id = do {_now <- getCurrentTime;
                                                        updateOneWithKV [Se.Set Beam.badgeName badgeName, Se.Set Beam.badgeRank badgeRank, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]}
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FleetBadge.FleetBadge -> m (Maybe Domain.Types.FleetBadge.FleetBadge))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetBadge.FleetBadge -> m ())
updateByPrimaryKey (Domain.Types.FleetBadge.FleetBadge {..}) = do {_now <- getCurrentTime;
                                                                   updateWithKV [Se.Set Beam.badgeName badgeName,
                                                                                 Se.Set Beam.badgeRank badgeRank,
                                                                                 Se.Set Beam.badgeType badgeType,
                                                                                 Se.Set Beam.fleetOwnerId (Kernel.Types.Id.getId fleetOwnerId),
                                                                                 Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
                                                                                 Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
                                                                                 Se.Set Beam.personId (Kernel.Types.Id.getId <$> personId),
                                                                                 Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



