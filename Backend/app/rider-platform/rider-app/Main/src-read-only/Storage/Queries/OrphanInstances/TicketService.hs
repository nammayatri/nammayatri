{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.TicketService where

import qualified Domain.Types.TicketService
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.TicketService as Beam

instance FromTType' Beam.TicketService Domain.Types.TicketService.TicketService where
  fromTType' (Beam.TicketServiceT {..}) = do
    pure $
      Just
        Domain.Types.TicketService.TicketService
          { allowCancellation = allowCancellation,
            allowFutureBooking = allowFutureBooking,
            businessHours = Kernel.Types.Id.Id <$> businessHours,
            expiry = expiry,
            id = Kernel.Types.Id.Id id,
            maxVerification = maxVerification,
            operationalDate = ((,) <$> operationalStartDate <*> operationalEndDate) <&> \(operationalStartDate', operationalEndDate') -> Domain.Types.TicketService.OperationalDate operationalEndDate' operationalStartDate',
            operationalDays = operationalDays,
            placesId = placesId,
            service = service,
            shortDesc = shortDesc,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.TicketService Domain.Types.TicketService.TicketService where
  toTType' (Domain.Types.TicketService.TicketService {..}) = do
    Beam.TicketServiceT
      { Beam.allowCancellation = allowCancellation,
        Beam.allowFutureBooking = allowFutureBooking,
        Beam.businessHours = Kernel.Types.Id.getId <$> businessHours,
        Beam.expiry = expiry,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.maxVerification = maxVerification,
        Beam.operationalEndDate = operationalDate <&> (.eneDate),
        Beam.operationalStartDate = operationalDate <&> (.startDate),
        Beam.operationalDays = operationalDays,
        Beam.placesId = placesId,
        Beam.service = service,
        Beam.shortDesc = shortDesc,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
