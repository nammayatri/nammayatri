{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.TicketBookingPeopleCategory where

import qualified Data.Aeson
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
          { amountToRefund = amountToRefund,
            id = Kernel.Types.Id.Id id,
            name = name,
            numberOfUnits = numberOfUnits,
            numberOfUnitsCancelled = numberOfUnitsCancelled,
            peopleCategoryId = Kernel.Types.Id.Id <$> peopleCategoryId,
            pricePerUnit = Kernel.Types.Common.mkPrice currency pricePerUnit,
            ticketBookingServiceCategoryId = Kernel.Types.Id.Id ticketBookingServiceCategoryId,
            vendorSplitDetails = (\val -> case Data.Aeson.fromJSON val of Data.Aeson.Success x -> Just x; Data.Aeson.Error _ -> Nothing) =<< vendorSplitDetails,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.TicketBookingPeopleCategory Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory where
  toTType' (Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory {..}) = do
    Beam.TicketBookingPeopleCategoryT
      { Beam.amountToRefund = amountToRefund,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.name = name,
        Beam.numberOfUnits = numberOfUnits,
        Beam.numberOfUnitsCancelled = numberOfUnitsCancelled,
        Beam.peopleCategoryId = Kernel.Types.Id.getId <$> peopleCategoryId,
        Beam.currency = (Kernel.Prelude.Just . (.currency)) pricePerUnit,
        Beam.pricePerUnit = (.amount) pricePerUnit,
        Beam.ticketBookingServiceCategoryId = Kernel.Types.Id.getId ticketBookingServiceCategoryId,
        Beam.vendorSplitDetails = Data.Aeson.toJSON <$> vendorSplitDetails,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
