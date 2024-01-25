{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.SearchRequestSpecialZone where

import Data.Ord
import Domain.Types.SearchRequest as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.SearchRequestSpecialZone as BeamSRSZ
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM

findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id SearchRequest -> m (Maybe SearchRequest)
findById (Id searchReqId) = findOneWithKV [Se.Is BeamSRSZ.id $ Se.Eq searchReqId]

instance FromTType' BeamSRSZ.SearchRequestSpecialZone SearchRequest where
  fromTType' BeamSRSZ.SearchRequestSpecialZoneT {..} = do
    mappings <- QLM.findByEntityId id
    (fl, tl) <- do
      let fromLocationMapping = filter (\loc -> loc.order == 0) mappings
          toLocationMappings = filter (\loc -> loc.order /= 0) mappings

      fromLocMap <- listToMaybe fromLocationMapping & fromMaybeM (InternalError "Entity Mappings For FromLocation Not Found")
      fl <- QL.findById fromLocMap.locationId >>= fromMaybeM (InternalError $ "FromLocation not found in booking for fromLocationId: " <> fromLocMap.locationId.getId)

      when (null toLocationMappings) $ throwError (InternalError "Entity Mappings For ToLocation Not Found")
      let toLoc = maximumBy (comparing (.order)) toLocationMappings
      tl <- QL.findById toLoc.locationId >>= fromMaybeM (InternalError $ "ToLocation not found in booking for toLocationId: " <> toLoc.locationId.getId)
      return (fl, tl)
    pUrl <- parseBaseUrl bapUri
    merchant <- CQM.findById (Id providerId) >>= fromMaybeM (MerchantNotFound providerId)
    merchantOpCityId <- CQMOC.getMerchantOpCityId (Id <$> merchantOperatingCityId) merchant Nothing
    pure $
      Just
        SearchRequest
          { id = Id id,
            transactionId = transactionId,
            messageId = Just messageId,
            startTime = startTime,
            providerId = Id providerId,
            merchantOperatingCityId = merchantOpCityId,
            fromLocation = fl,
            toLocation = Just tl,
            area = area,
            bapId = bapId,
            bapUri = pUrl,
            bapCity = Nothing,
            bapCountry = Nothing,
            specialLocationTag = Nothing,
            autoAssignEnabled = Nothing,
            device = Nothing,
            customerLanguage = Nothing,
            disabilityTag = Nothing,
            customerCancellationDues = 0,
            isReallocationEnabled = Nothing,
            estimatedDistance = Just estimatedDistance,
            estimatedDuration = Just estimatedDuration,
            createdAt = createdAt
          }

instance ToTType' BeamSRSZ.SearchRequestSpecialZone SearchRequest where
  toTType' SearchRequest {..} = do
    BeamSRSZ.SearchRequestSpecialZoneT
      { BeamSRSZ.id = getId id,
        BeamSRSZ.transactionId = transactionId,
        BeamSRSZ.messageId = fromMaybe "" messageId, -- JUST TEMPORARY: ANY WAY NO USE
        BeamSRSZ.startTime = startTime,
        BeamSRSZ.validTill = startTime, -- JUST TEMPORARY: ANY WAY NO USE
        BeamSRSZ.providerId = getId providerId,
        BeamSRSZ.merchantOperatingCityId = Just $ getId merchantOperatingCityId,
        BeamSRSZ.fromLocationId = Just $ getId fromLocation.id,
        BeamSRSZ.toLocationId = (getId . (.id)) <$> toLocation,
        BeamSRSZ.area = area,
        BeamSRSZ.bapId = bapId,
        BeamSRSZ.bapUri = showBaseUrl bapUri,
        BeamSRSZ.estimatedDistance = fromMaybe 0 estimatedDistance, -- JUST TEMPORARY: ANY WAY NO USE
        BeamSRSZ.estimatedDuration = fromMaybe 0 estimatedDuration, -- JUST TEMPORARY: ANY WAY NO USE
        BeamSRSZ.createdAt = createdAt,
        BeamSRSZ.updatedAt = createdAt
      }
