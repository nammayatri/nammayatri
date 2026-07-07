module Storage.Queries.FleetRCAssociationExtra where

import Domain.Types.FleetRCAssociation
import Domain.Types.Person (Person)
import Domain.Types.VehicleRegistrationCertificate
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetRCAssociation as Beam
import Storage.Queries.OrphanInstances.FleetRCAssociation ()

-- Extra code goes here --
findLinkedByRCIdAndFleetOwnerId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Id VehicleRegistrationCertificate -> UTCTime -> m (Maybe FleetRCAssociation)
findLinkedByRCIdAndFleetOwnerId (Id fleetOwnerId) (Id rcId) now = findOneWithKV [Se.And [Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId, Se.Is Beam.rcId $ Se.Eq rcId, Se.Is Beam.associatedTill $ Se.GreaterThan $ Just now]]

findLatestByRCIdAndFleetOwnerId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id VehicleRegistrationCertificate -> Id Person -> m (Maybe FleetRCAssociation)
findLatestByRCIdAndFleetOwnerId (Id rcId) (Id fleetOwnerId) =
  findAllWithOptionsKV [Se.And [Se.Is Beam.rcId $ Se.Eq rcId, Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId]] (Se.Desc Beam.associatedTill) (Just 1) Nothing <&> listToMaybe

endAssociationForRC :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Id VehicleRegistrationCertificate -> m ()
endAssociationForRC (Id fleetOwnerId) (Id rcId) = do
  now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.associatedTill $ Just now]
    [Se.And [Se.Is Beam.fleetOwnerId (Se.Eq fleetOwnerId), Se.Is Beam.associatedTill (Se.GreaterThan $ Just now), Se.Is Beam.rcId (Se.Eq rcId)]]

findAllActiveAssociationByRCId ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id VehicleRegistrationCertificate ->
  m [FleetRCAssociation]
findAllActiveAssociationByRCId (Id rcId) = do
  now <- getCurrentTime
  -- Order by associatedOn DESC so callers taking the head (listToMaybe) deterministically get the most
  -- recent active fleet association, even when an RC transiently has more than one during reassignment.
  findAllWithOptionsKV [Se.And [Se.Is Beam.rcId $ Se.Eq rcId, Se.Is Beam.associatedTill $ Se.GreaterThan $ Just now]] (Se.Desc Beam.associatedOn) Nothing Nothing

findActiveAssociationByFleetOwnerId ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Person ->
  m (Maybe FleetRCAssociation)
findActiveAssociationByFleetOwnerId (Id fleetOwnerId) = do
  now <- getCurrentTime
  listToMaybe
    <$> findAllWithOptionsKV'
      [Se.And [Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId, Se.Is Beam.associatedTill $ Se.GreaterThan $ Just now]]
      (Just 1)
      Nothing

deleteByFleetOwnerId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m ()
deleteByFleetOwnerId (Id fleetOwnerId) = deleteWithKV [Se.Is Beam.fleetOwnerId (Se.Eq fleetOwnerId)]

deleteById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id FleetRCAssociation -> m ()
deleteById rowId = deleteWithKV [Se.Is Beam.id $ Se.Eq rowId.getId]

-- Mark the fleet-vehicle association ended (associatedTill=now). This UPDATE
-- streams to ClickHouse (drainer -> Kafka) as the terminal history event; the
-- caller then hard-deletes the row. Hard deletes are NOT pushed to Kafka.
endById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id FleetRCAssociation -> m ()
endById rowId = do
  now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.associatedTill (Just now), Se.Set Beam.updatedAt now]
    [Se.Is Beam.id $ Se.Eq rowId.getId]
