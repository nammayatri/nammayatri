{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.QuoteSpecialZone where

import qualified Data.Time as T
import qualified Database.Beam as B
import Database.Beam.Postgres
import Domain.Types.QuoteSpecialZone
import Domain.Types.SearchRequestSpecialZone
import EulerHS.KVConnector.Utils (meshModelTableEntity)
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Logging
import Lib.Utils (FromTType' (fromTType'), ToTType' (toTType'), createWithKV, findOneWithKV, getMasterBeamConfig)
import Sequelize
import qualified Sequelize as Se
import qualified Storage.Beam.QuoteSpecialZone as BeamQSZ
import Storage.Queries.FareParameters as BeamQFP

-- create :: QuoteSpecialZone -> SqlDB ()
-- create quote = Esq.runTransaction $
--   withFullEntity quote $ \(quoteT, (fareParams', fareParamsDetais)) -> do
--     Esq.create' fareParams'
--       FareParamsT.ProgressiveDetailsT fppdt -> Esq.create' fppdt
--       FareParamsT.SlabDetailsT -> return ()
--     Esq.create' quoteT

create :: (L.MonadFlow m, Log m) => QuoteSpecialZone -> m ()
create = createWithKV

countAllByRequestId :: (L.MonadFlow m, Log m) => Id SearchRequestSpecialZone -> m Int
countAllByRequestId searchReqID = do
  dbConf <- getMasterBeamConfig
  resp <-
    L.runDB dbConf $
      L.findRow $
        B.select $
          B.aggregate_ (\_ -> B.as_ @Int B.countAll_) $
            B.filter_' (\(BeamQSZ.QuoteSpecialZoneT {..}) -> searchRequestId B.==?. B.val_ (getId searchReqID)) $
              B.all_ (meshModelTableEntity @BeamQSZ.QuoteSpecialZoneT @Postgres @(DatabaseWith BeamQSZ.QuoteSpecialZoneT))
  pure (either (const 0) (fromMaybe 0) resp)

findById :: (L.MonadFlow m, Log m) => Id QuoteSpecialZone -> m (Maybe QuoteSpecialZone)
findById (Id dQuoteId) = findOneWithKV [Se.Is BeamQSZ.id $ Se.Eq dQuoteId]

instance FromTType' BeamQSZ.QuoteSpecialZone QuoteSpecialZone where
  fromTType' BeamQSZ.QuoteSpecialZoneT {..} = do
    fp <- BeamQFP.findById (Id fareParametersId)
    if isJust fp
      then
        pure $
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
                fareParams = fromJust fp -- to take a default value?
              }
      else pure Nothing

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
