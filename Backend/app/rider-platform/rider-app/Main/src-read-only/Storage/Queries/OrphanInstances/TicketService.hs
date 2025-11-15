{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.TicketService where

import qualified Data.Aeson
import qualified Domain.Types.TicketService
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
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
            isClosed = fromMaybe False isClosed,
            maxSelection = maxSelection,
            maxVerification = maxVerification,
            note = note,
            operationalDate = ((,) <$> operationalStartDate <*> operationalEndDate) <&> \(operationalStartDate', operationalEndDate') -> Domain.Types.TicketService.OperationalDate operationalEndDate' operationalStartDate',
            operationalDays = operationalDays,
            placesId = placesId,
            priority = priority,
            rules = (\val -> case Data.Aeson.fromJSON val of Data.Aeson.Success x -> Just x; Data.Aeson.Error _ -> Nothing) =<< rules,
            service = service,
            serviceDetails = (\val -> case Data.Aeson.fromJSON val of Data.Aeson.Success x -> Just x; Data.Aeson.Error _ -> Nothing) =<< serviceDetails,
            shortDesc = shortDesc,
            subPlaceId = Kernel.Types.Id.Id <$> subPlaceId,
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
        Beam.isClosed = Kernel.Prelude.Just isClosed,
        Beam.maxSelection = maxSelection,
        Beam.maxVerification = maxVerification,
        Beam.note = note,
        Beam.operationalEndDate = operationalDate <&> (.eneDate),
        Beam.operationalStartDate = operationalDate <&> (.startDate),
        Beam.operationalDays = operationalDays,
        Beam.placesId = placesId,
        Beam.priority = priority,
        Beam.rules = Data.Aeson.toJSON <$> rules,
        Beam.service = service,
        Beam.serviceDetails = Data.Aeson.toJSON <$> serviceDetails,
        Beam.shortDesc = shortDesc,
        Beam.subPlaceId = Kernel.Types.Id.getId <$> subPlaceId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
