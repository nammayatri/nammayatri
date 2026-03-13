{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.CorporateEmployee where

import qualified Domain.Types.CorporateEmployee
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.CorporateEmployee as Beam

instance FromTType' Beam.CorporateEmployee Domain.Types.CorporateEmployee.CorporateEmployee where
  fromTType' (Beam.CorporateEmployeeT {..}) = do
    pure $
      Just
        Domain.Types.CorporateEmployee.CorporateEmployee
          { id = Kernel.Types.Id.Id id,
            corporateEntityId = Kernel.Types.Id.Id corporateEntityId,
            personId = Kernel.Types.Id.Id <$> personId,
            employeeCode = employeeCode,
            name = name,
            email = email,
            phone = phone,
            department = department,
            costCenter = costCenter,
            gender = Kernel.Prelude.fromMaybe Domain.Types.Person.UNKNOWN (Kernel.Prelude.readMaybe (Kernel.Prelude.toString gender)),
            officeLocationId = officeLocationId,
            defaultPickupLat = defaultPickupLat,
            defaultPickupLon = defaultPickupLon,
            defaultPickupAddress = defaultPickupAddress,
            reportingManagerEmail = reportingManagerEmail,
            status = Kernel.Prelude.fromMaybe Domain.Types.CorporateEmployee.ACTIVE (Kernel.Prelude.readMaybe (Kernel.Prelude.toString status)),
            linkedAt = linkedAt,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.CorporateEmployee Domain.Types.CorporateEmployee.CorporateEmployee where
  toTType' (Domain.Types.CorporateEmployee.CorporateEmployee {..}) = do
    Beam.CorporateEmployeeT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.corporateEntityId = Kernel.Types.Id.getId corporateEntityId,
        Beam.personId = Kernel.Types.Id.getId <$> personId,
        Beam.employeeCode = employeeCode,
        Beam.name = name,
        Beam.email = email,
        Beam.phone = phone,
        Beam.department = department,
        Beam.costCenter = costCenter,
        Beam.gender = Kernel.Prelude.show gender,
        Beam.officeLocationId = officeLocationId,
        Beam.defaultPickupLat = defaultPickupLat,
        Beam.defaultPickupLon = defaultPickupLon,
        Beam.defaultPickupAddress = defaultPickupAddress,
        Beam.reportingManagerEmail = reportingManagerEmail,
        Beam.status = Kernel.Prelude.show status,
        Beam.linkedAt = linkedAt,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
