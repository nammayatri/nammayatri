{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.MerchantOperatingCity where

import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.MerchantOperatingCity as Beam

instance FromTType' Beam.MerchantOperatingCity Domain.Types.MerchantOperatingCity.MerchantOperatingCity where
  fromTType' (Beam.MerchantOperatingCityT {..}) = do
    pure $
      Just
        Domain.Types.MerchantOperatingCity.MerchantOperatingCity
          { city = city,
            country = country,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            driverOfferMerchantOperatingCityId = driverOfferMerchantOperatingCityId,
            id = Kernel.Types.Id.Id id,
            lat = lat,
            long = long,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantShortId = Kernel.Types.Id.ShortId merchantShortId,
            state = state,
            stdCode = stdCode,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.MerchantOperatingCity Domain.Types.MerchantOperatingCity.MerchantOperatingCity where
  toTType' (Domain.Types.MerchantOperatingCity.MerchantOperatingCity {..}) = do
    Beam.MerchantOperatingCityT
      { Beam.city = city,
        Beam.country = country,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.driverOfferMerchantOperatingCityId = driverOfferMerchantOperatingCityId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lat = lat,
        Beam.long = long,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantShortId = Kernel.Types.Id.getShortId merchantShortId,
        Beam.state = state,
        Beam.stdCode = stdCode,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
