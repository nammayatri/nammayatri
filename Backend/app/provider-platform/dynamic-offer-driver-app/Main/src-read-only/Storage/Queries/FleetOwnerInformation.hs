{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetOwnerInformation where

import qualified Domain.Types.FleetOwnerInformation
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetOwnerInformation as Beam

create :: KvDbFlow m r => (Domain.Types.FleetOwnerInformation.FleetOwnerInformation -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.FleetOwnerInformation.FleetOwnerInformation] -> m ())
createMany = traverse_ create

updateFleetOwnerGstNumberAndEnabledStatus :: KvDbFlow m r => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateFleetOwnerGstNumberAndEnabledStatus gstNumber enabled (Kernel.Types.Id.Id fleetOwnerPersonId) = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.gstNumber gstNumber, Se.Set Beam.enabled enabled, Se.Set Beam.updatedAt _now] [Se.Is Beam.fleetOwnerPersonId $ Se.Eq fleetOwnerPersonId]

updateFleetOwnerVerifiedStatus :: KvDbFlow m r => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateFleetOwnerVerifiedStatus verified (Kernel.Types.Id.Id fleetOwnerPersonId) = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.verified verified, Se.Set Beam.updatedAt _now] [Se.Is Beam.fleetOwnerPersonId $ Se.Eq fleetOwnerPersonId]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.FleetOwnerInformation.FleetOwnerInformation))
findByPrimaryKey (Kernel.Types.Id.Id fleetOwnerPersonId) = do findOneWithKV [Se.And [Se.Is Beam.fleetOwnerPersonId $ Se.Eq fleetOwnerPersonId]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.FleetOwnerInformation.FleetOwnerInformation -> m ())
updateByPrimaryKey (Domain.Types.FleetOwnerInformation.FleetOwnerInformation {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.blocked blocked,
      Se.Set Beam.enabled enabled,
      Se.Set Beam.fleetType fleetType,
      Se.Set Beam.gstNumber gstNumber,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.verified verified,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.fleetOwnerPersonId $ Se.Eq (Kernel.Types.Id.getId fleetOwnerPersonId)]]

instance FromTType' Beam.FleetOwnerInformation Domain.Types.FleetOwnerInformation.FleetOwnerInformation where
  fromTType' (Beam.FleetOwnerInformationT {..}) = do
    pure $
      Just
        Domain.Types.FleetOwnerInformation.FleetOwnerInformation
          { blocked = blocked,
            enabled = enabled,
            fleetOwnerPersonId = Kernel.Types.Id.Id fleetOwnerPersonId,
            fleetType = fleetType,
            gstNumber = gstNumber,
            merchantId = Kernel.Types.Id.Id merchantId,
            verified = verified,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FleetOwnerInformation Domain.Types.FleetOwnerInformation.FleetOwnerInformation where
  toTType' (Domain.Types.FleetOwnerInformation.FleetOwnerInformation {..}) = do
    Beam.FleetOwnerInformationT
      { Beam.blocked = blocked,
        Beam.enabled = enabled,
        Beam.fleetOwnerPersonId = Kernel.Types.Id.getId fleetOwnerPersonId,
        Beam.fleetType = fleetType,
        Beam.gstNumber = gstNumber,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.verified = verified,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
