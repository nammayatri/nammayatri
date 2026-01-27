{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.MerchantOperatingCity where

import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Maps.Types
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
            currency = fromMaybe Kernel.Types.Common.INR currency,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            id = Kernel.Types.Id.Id id,
            language = language,
            location = Kernel.External.Maps.Types.LatLong lat lon,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantShortId = Kernel.Types.Id.ShortId merchantShortId,
            state = state,
            stdCode = stdCode,
            supportNumber = supportNumber
          }

instance ToTType' Beam.MerchantOperatingCity Domain.Types.MerchantOperatingCity.MerchantOperatingCity where
  toTType' (Domain.Types.MerchantOperatingCity.MerchantOperatingCity {..}) = do
    Beam.MerchantOperatingCityT
      { Beam.city = city,
        Beam.country = country,
        Beam.currency = Just currency,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.language = language,
        Beam.lat = (.lat) location,
        Beam.lon = (.lon) location,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantShortId = Kernel.Types.Id.getShortId merchantShortId,
        Beam.state = state,
        Beam.stdCode = stdCode,
        Beam.supportNumber = supportNumber
      }
