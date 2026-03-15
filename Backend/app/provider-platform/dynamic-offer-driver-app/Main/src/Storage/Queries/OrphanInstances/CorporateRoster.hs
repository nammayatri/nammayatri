{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.CorporateRoster where

import qualified Domain.Types.CorporateRoster
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.CorporateRoster as Beam

instance FromTType' Beam.CorporateRoster Domain.Types.CorporateRoster.CorporateRoster where
  fromTType' (Beam.CorporateRosterT {..}) = do
    pure $
      Just
        Domain.Types.CorporateRoster.CorporateRoster
          { id = Kernel.Types.Id.Id id,
            corporateEntityId = corporateEntityId,
            corporateEmployeeId = corporateEmployeeId,
            corporateShiftId = corporateShiftId,
            corporateRouteId = corporateRouteId,
            rosterDate = rosterDate,
            attendanceStatus = Kernel.Prelude.fromMaybe Domain.Types.CorporateRoster.SCHEDULED (Kernel.Prelude.readMaybe (Kernel.Prelude.toString attendanceStatus)),
            bookingId = bookingId,
            confirmedAt = confirmedAt,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.CorporateRoster Domain.Types.CorporateRoster.CorporateRoster where
  toTType' (Domain.Types.CorporateRoster.CorporateRoster {..}) = do
    Beam.CorporateRosterT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.corporateEntityId = corporateEntityId,
        Beam.corporateEmployeeId = corporateEmployeeId,
        Beam.corporateShiftId = corporateShiftId,
        Beam.corporateRouteId = corporateRouteId,
        Beam.rosterDate = rosterDate,
        Beam.attendanceStatus = Kernel.Prelude.show attendanceStatus,
        Beam.bookingId = bookingId,
        Beam.confirmedAt = confirmedAt,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
