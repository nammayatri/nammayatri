{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.CorporateShift where

import qualified Domain.Types.CorporateShift
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.CorporateShift as Beam

instance FromTType' Beam.CorporateShift Domain.Types.CorporateShift.CorporateShift where
  fromTType' (Beam.CorporateShiftT {..}) = do
    pure $
      Just
        Domain.Types.CorporateShift.CorporateShift
          { id = Kernel.Types.Id.Id id,
            corporateEntityId = Kernel.Types.Id.Id corporateEntityId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            name = name,
            pickupWindowStart = Kernel.Prelude.fromMaybe (error "Invalid pickupWindowStart") (Kernel.Prelude.readMaybe (Kernel.Prelude.toString pickupWindowStart)),
            pickupWindowEnd = Kernel.Prelude.fromMaybe (error "Invalid pickupWindowEnd") (Kernel.Prelude.readMaybe (Kernel.Prelude.toString pickupWindowEnd)),
            dropWindowStart = Kernel.Prelude.fromMaybe (error "Invalid dropWindowStart") (Kernel.Prelude.readMaybe (Kernel.Prelude.toString dropWindowStart)),
            dropWindowEnd = Kernel.Prelude.fromMaybe (error "Invalid dropWindowEnd") (Kernel.Prelude.readMaybe (Kernel.Prelude.toString dropWindowEnd)),
            activeDays = activeDays,
            isNightShift = isNightShift,
            maxOccupancy = maxOccupancy,
            allowedVehicleTiers = allowedVehicleTiers,
            confirmationDeadlineMinutes = confirmationDeadlineMinutes,
            status = Kernel.Prelude.fromMaybe Domain.Types.CorporateShift.CS_ACTIVE (Kernel.Prelude.readMaybe (Kernel.Prelude.toString status)),
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.CorporateShift Domain.Types.CorporateShift.CorporateShift where
  toTType' (Domain.Types.CorporateShift.CorporateShift {..}) = do
    Beam.CorporateShiftT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.corporateEntityId = Kernel.Types.Id.getId corporateEntityId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.name = name,
        Beam.pickupWindowStart = Kernel.Prelude.show pickupWindowStart,
        Beam.pickupWindowEnd = Kernel.Prelude.show pickupWindowEnd,
        Beam.dropWindowStart = Kernel.Prelude.show dropWindowStart,
        Beam.dropWindowEnd = Kernel.Prelude.show dropWindowEnd,
        Beam.activeDays = activeDays,
        Beam.isNightShift = isNightShift,
        Beam.maxOccupancy = maxOccupancy,
        Beam.allowedVehicleTiers = allowedVehicleTiers,
        Beam.confirmationDeadlineMinutes = confirmationDeadlineMinutes,
        Beam.status = Kernel.Prelude.show status,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
