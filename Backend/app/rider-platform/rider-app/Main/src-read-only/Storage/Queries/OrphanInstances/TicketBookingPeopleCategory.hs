{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.TicketBookingPeopleCategory where

import qualified Domain.Types.TicketBookingPeopleCategory
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.TicketBookingPeopleCategory as Beam

instance FromTType' Beam.TicketBookingPeopleCategory Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory where
  fromTType' (Beam.TicketBookingPeopleCategoryT {..}) = do
    pure $
      Just
        Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory
          { id = Kernel.Types.Id.Id id,
            ticketBookingServiceCategoryId = Kernel.Types.Id.Id ticketBookingServiceCategoryId,
            peopleCategoryId = Kernel.Types.Id.Id <$> peopleCategoryId,
            name = name,
            pricePerUnit = Kernel.Types.Common.mkPrice currency pricePerUnit,
            numberOfUnits = numberOfUnits,
            numberOfUnitsCancelled = numberOfUnitsCancelled,
            amountToRefund = amountToRefund,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.TicketBookingPeopleCategory Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory where
  toTType' (Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory {..}) = do
    Beam.TicketBookingPeopleCategoryT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.ticketBookingServiceCategoryId = Kernel.Types.Id.getId ticketBookingServiceCategoryId,
        Beam.peopleCategoryId = Kernel.Types.Id.getId <$> peopleCategoryId,
        Beam.name = name,
        Beam.currency = (Kernel.Prelude.Just . (.currency)) pricePerUnit,
        Beam.pricePerUnit = (.amount) pricePerUnit,
        Beam.numberOfUnits = numberOfUnits,
        Beam.numberOfUnitsCancelled = numberOfUnitsCancelled,
        Beam.amountToRefund = amountToRefund,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
