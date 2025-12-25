{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Quote where

import qualified Data.Time as T
import qualified Domain.Types as DTC
import Domain.Types.Quote
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Quote as BeamQSZ
import Storage.Cac.FarePolicy as BeamFPolicy
import Storage.Queries.FareParameters as BeamQFP
import qualified Storage.Queries.FareParameters as SQFP

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Quote -> m ()
create quote = SQFP.create quote.fareParams >> createWithKV quote

findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Quote -> m (Maybe Quote)
findById (Id dQuoteId) = findOneWithKV [Se.Is BeamQSZ.id $ Se.Eq dQuoteId]

{-
  The table named quote_special_zone in the QuoteT database was originally created exclusively for handling
  data pertaining to special zones. This naming convention has been retained for reasons of backward compatibility.
  Over time, the use of this table was expanded beyond its initial scope to accommodate a broader range of use cases,
  extending its functionality beyond just special zones
-}
instance FromTType' BeamQSZ.QuoteSpecialZone Quote where
  fromTType' BeamQSZ.QuoteSpecialZoneT {..} = do
    farePolicy <- maybe (pure Nothing) (BeamFPolicy.findById Nothing . Id) farePolicyId
    fareParams <- BeamQFP.findById (Id fareParametersId) >>= fromMaybeM (InternalError $ "FareParameters not found in Quote for id: " <> show fareParametersId)
    return $
      Just
        Quote
          { id = Id id,
            searchRequestId = Id searchRequestId,
            providerId = Id providerId,
            tripCategory = fromMaybe (DTC.OneWay DTC.OneWayRideOtp) tripCategory,
            isScheduled = fromMaybe False isScheduled,
            vehicleServiceTier = vehicleVariant,
            createdAt = T.localTimeToUTC T.utc createdAt,
            updatedAt = T.localTimeToUTC T.utc updatedAt,
            validTill = T.localTimeToUTC T.utc validTill,
            estimatedFare = mkAmountWithDefault estimatedFareAmount estimatedFare,
            currency = fromMaybe INR currency,
            distanceUnit = fromMaybe Meter distanceUnit,
            merchantOperatingCityId = Id <$> merchantOperatingCityId,
            ..
          }

instance ToTType' BeamQSZ.QuoteSpecialZone Quote where
  toTType' Quote {..} = do
    BeamQSZ.QuoteSpecialZoneT
      { BeamQSZ.id = getId id,
        BeamQSZ.searchRequestId = getId searchRequestId,
        BeamQSZ.providerId = getId providerId,
        BeamQSZ.vehicleVariant = vehicleServiceTier,
        BeamQSZ.estimatedFinishTime = estimatedFinishTime,
        BeamQSZ.tripCategory = Just tripCategory,
        BeamQSZ.vehicleServiceTierName = vehicleServiceTierName,
        BeamQSZ.distance = distance,
        BeamQSZ.distanceUnit = Just distanceUnit,
        BeamQSZ.createdAt = T.utcToLocalTime T.utc createdAt,
        BeamQSZ.updatedAt = T.utcToLocalTime T.utc updatedAt,
        BeamQSZ.validTill = T.utcToLocalTime T.utc validTill,
        BeamQSZ.estimatedFare = roundToIntegral estimatedFare,
        BeamQSZ.estimatedFareAmount = Just estimatedFare,
        BeamQSZ.currency = Just currency,
        BeamQSZ.specialLocationTag = specialLocationTag,
        BeamQSZ.fareParametersId = getId fareParams.id,
        BeamQSZ.isScheduled = Just isScheduled,
        BeamQSZ.isCustomerPrefferedSearchRoute = isCustomerPrefferedSearchRoute,
        BeamQSZ.isBlockedRoute = isBlockedRoute,
        BeamQSZ.tollNames = tollNames,
        BeamQSZ.farePolicyId = getId . (.id) <$> farePolicy,
        BeamQSZ.merchantOperatingCityId = getId <$> merchantOperatingCityId
      }
