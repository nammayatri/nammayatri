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
            corporateEntityId = Kernel.Types.Id.Id corporateEntityId,
            corporateEmployeeId = Kernel.Types.Id.Id corporateEmployeeId,
            corporateShiftId = Kernel.Types.Id.Id corporateShiftId,
            corporateRouteId = Kernel.Types.Id.Id <$> corporateRouteId,
            corporateRouteStopId = Kernel.Types.Id.Id <$> corporateRouteStopId,
            rosterDate = rosterDate,
            attendanceStatus = Kernel.Prelude.fromMaybe Domain.Types.CorporateRoster.SCHEDULED (Kernel.Prelude.readMaybe (Kernel.Prelude.toString attendanceStatus)),
            bookingId = Kernel.Types.Id.Id <$> bookingId,
            confirmedAt = confirmedAt,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.CorporateRoster Domain.Types.CorporateRoster.CorporateRoster where
  toTType' (Domain.Types.CorporateRoster.CorporateRoster {..}) = do
    Beam.CorporateRosterT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.corporateEntityId = Kernel.Types.Id.getId corporateEntityId,
        Beam.corporateEmployeeId = Kernel.Types.Id.getId corporateEmployeeId,
        Beam.corporateShiftId = Kernel.Types.Id.getId corporateShiftId,
        Beam.corporateRouteId = Kernel.Types.Id.getId <$> corporateRouteId,
        Beam.corporateRouteStopId = Kernel.Types.Id.getId <$> corporateRouteStopId,
        Beam.rosterDate = rosterDate,
        Beam.attendanceStatus = Kernel.Prelude.show attendanceStatus,
        Beam.bookingId = Kernel.Types.Id.getId <$> bookingId,
        Beam.confirmedAt = confirmedAt,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
