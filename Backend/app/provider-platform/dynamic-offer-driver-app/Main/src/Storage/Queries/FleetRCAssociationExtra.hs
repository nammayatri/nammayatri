{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetRCAssociationExtra where

import Domain.Types.FleetRCAssociation
import Domain.Types.Person (Person)
import Domain.Types.VehicleRegistrationCertificate
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetRCAssociation
import qualified Storage.Beam.FleetRCAssociation as Beam
import Storage.Queries.OrphanInstances.FleetRCAssociation

-- Extra code goes here --
findLinkedByRCIdAndFleetOwnerId :: KvDbFlow m r => Id Person -> Id VehicleRegistrationCertificate -> UTCTime -> m (Maybe FleetRCAssociation)
findLinkedByRCIdAndFleetOwnerId (Id fleetOwnerId) (Id rcId) now = findOneWithKV [Se.And [Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId, Se.Is Beam.rcId $ Se.Eq rcId, Se.Is Beam.associatedTill $ Se.GreaterThan $ Just now]]

findLatestByRCIdAndFleetOwnerId :: KvDbFlow m r => Id VehicleRegistrationCertificate -> Id Person -> m (Maybe FleetRCAssociation)
findLatestByRCIdAndFleetOwnerId (Id rcId) (Id fleetOwnerId) =
  findAllWithOptionsKV [Se.And [Se.Is Beam.rcId $ Se.Eq rcId, Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId]] (Se.Desc Beam.associatedTill) (Just 1) Nothing <&> listToMaybe

endAssociationForRC :: KvDbFlow m r => Id Person -> Id VehicleRegistrationCertificate -> m ()
endAssociationForRC (Id fleetOwnerId) (Id rcId) = do
  now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.associatedTill $ Just now]
    [Se.And [Se.Is Beam.fleetOwnerId (Se.Eq fleetOwnerId), Se.Is Beam.associatedTill (Se.GreaterThan $ Just now), Se.Is Beam.rcId (Se.Eq rcId)]]
