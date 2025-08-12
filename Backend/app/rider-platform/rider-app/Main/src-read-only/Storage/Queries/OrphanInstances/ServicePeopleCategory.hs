{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.ServicePeopleCategory where

import qualified Data.Aeson
import qualified Domain.Types.ServicePeopleCategory
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import qualified Kernel.Types.TimeBound
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.ServicePeopleCategory as Beam
import Storage.Queries.Transformers.ServicePeopleCategory

instance FromTType' Beam.ServicePeopleCategory Domain.Types.ServicePeopleCategory.ServicePeopleCategory where
  fromTType' (Beam.ServicePeopleCategoryT {..}) = do
    pure $
      Just
        Domain.Types.ServicePeopleCategory.ServicePeopleCategory
          { cancellationCharges = getCancellationChargesFromTable cancellationCharges,
            description = description,
            iconUrl = iconUrl,
            id = Kernel.Types.Id.Id id,
            isClosed = fromMaybe False isClosed,
            name = name,
            placeId = placeId,
            pricePerUnit = Kernel.Types.Common.mkPrice currency pricePerUnit,
            pricingType = Kernel.Prelude.fromMaybe Domain.Types.ServicePeopleCategory.AllDays pricingType,
            rules = (\val -> case Data.Aeson.fromJSON val of Data.Aeson.Success x -> Just x; Data.Aeson.Error _ -> Nothing) =<< rules,
            timeBounds = Kernel.Prelude.fromMaybe Kernel.Types.TimeBound.Unbounded timeBounds,
            vendorSplitDetails = (\val -> case Data.Aeson.fromJSON val of Data.Aeson.Success x -> Just x; Data.Aeson.Error _ -> Nothing) =<< vendorSplitDetails,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.ServicePeopleCategory Domain.Types.ServicePeopleCategory.ServicePeopleCategory where
  toTType' (Domain.Types.ServicePeopleCategory.ServicePeopleCategory {..}) = do
    Beam.ServicePeopleCategoryT
      { Beam.cancellationCharges = convertCancellationChargesToTable cancellationCharges,
        Beam.description = description,
        Beam.iconUrl = iconUrl,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isClosed = Kernel.Prelude.Just isClosed,
        Beam.name = name,
        Beam.placeId = placeId,
        Beam.currency = (Kernel.Prelude.Just . (.currency)) pricePerUnit,
        Beam.pricePerUnit = (.amount) pricePerUnit,
        Beam.pricingType = Kernel.Prelude.Just pricingType,
        Beam.rules = Data.Aeson.toJSON <$> rules,
        Beam.timeBounds = Kernel.Prelude.Just timeBounds,
        Beam.vendorSplitDetails = Data.Aeson.toJSON <$> vendorSplitDetails,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
