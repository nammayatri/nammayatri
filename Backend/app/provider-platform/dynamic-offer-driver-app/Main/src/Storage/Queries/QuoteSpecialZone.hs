{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.QuoteSpecialZone where

import qualified Data.Time as T
import qualified Database.Beam as B
import Domain.Types.QuoteSpecialZone
import Domain.Types.SearchRequestSpecialZone
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.QuoteSpecialZone as BeamQSZ
import Storage.Queries.FareParameters as BeamQFP
import qualified Storage.Queries.FareParameters as SQFP

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => QuoteSpecialZone -> m ()
create quote = SQFP.create quote.fareParams >> createWithKV quote

countAllByRequestId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id SearchRequestSpecialZone -> m Int
countAllByRequestId searchReqID = do
  dbConf <- getMasterBeamConfig
  resp <-
    L.runDB dbConf $
      L.findRow $
        B.select $
          B.aggregate_ (\_ -> B.as_ @Int B.countAll_) $
            B.filter_' (\(BeamQSZ.QuoteSpecialZoneT {..}) -> searchRequestId B.==?. B.val_ (getId searchReqID)) $
              B.all_ (BeamCommon.quoteSpecialZone BeamCommon.atlasDB)
  pure (either (const 0) (fromMaybe 0) resp)

findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id QuoteSpecialZone -> m (Maybe QuoteSpecialZone)
findById (Id dQuoteId) = findOneWithKV [Se.Is BeamQSZ.id $ Se.Eq dQuoteId]

instance FromTType' BeamQSZ.QuoteSpecialZone QuoteSpecialZone where
  fromTType' BeamQSZ.QuoteSpecialZoneT {..} = do
    fp <- BeamQFP.findById (Id fareParametersId) >>= fromMaybeM (InternalError $ "FareParameters not found in QuoteSpecialZone for id: " <> show fareParametersId)
    return $
      Just
        QuoteSpecialZone
          { id = Id id,
            searchRequestId = Id searchRequestId,
            providerId = Id providerId,
            vehicleVariant = vehicleVariant,
            distance = distance,
            estimatedFinishTime = estimatedFinishTime,
            createdAt = T.localTimeToUTC T.utc createdAt,
            updatedAt = T.localTimeToUTC T.utc updatedAt,
            validTill = T.localTimeToUTC T.utc validTill,
            estimatedFare = estimatedFare,
            specialLocationTag = specialLocationTag,
            fareParams = fp
          }

instance ToTType' BeamQSZ.QuoteSpecialZone QuoteSpecialZone where
  toTType' QuoteSpecialZone {..} = do
    BeamQSZ.QuoteSpecialZoneT
      { BeamQSZ.id = getId id,
        BeamQSZ.searchRequestId = getId searchRequestId,
        BeamQSZ.providerId = getId providerId,
        BeamQSZ.vehicleVariant = vehicleVariant,
        BeamQSZ.distance = distance,
        BeamQSZ.estimatedFinishTime = estimatedFinishTime,
        BeamQSZ.createdAt = T.utcToLocalTime T.utc createdAt,
        BeamQSZ.updatedAt = T.utcToLocalTime T.utc updatedAt,
        BeamQSZ.validTill = T.utcToLocalTime T.utc validTill,
        BeamQSZ.estimatedFare = estimatedFare,
        BeamQSZ.specialLocationTag = specialLocationTag,
        BeamQSZ.fareParametersId = getId fareParams.id
      }
