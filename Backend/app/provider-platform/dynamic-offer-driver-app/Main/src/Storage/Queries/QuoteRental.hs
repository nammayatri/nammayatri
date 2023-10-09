{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.QuoteRental where

import qualified Data.Time as T
import qualified Database.Beam as B
import Domain.Types.QuoteRental
import Domain.Types.SearchRequestRental
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.QuoteRental as BeamQR
import Storage.Queries.FareParameters as BeamQFP
import qualified Storage.Queries.FareParameters as SQFP

create :: MonadFlow m => QuoteRental -> m ()
create quote = SQFP.create quote.fareParams >> createWithKV quote

countAllByRequestId :: MonadFlow m => Id SearchRequestRental -> m Int
countAllByRequestId searchReqID = do
  dbConf <- getMasterBeamConfig
  resp <-
    L.runDB dbConf $
      L.findRow $
        B.select $
          B.aggregate_ (\_ -> B.as_ @Int B.countAll_) $
            B.filter_' (\(BeamQR.QuoteRentalT {..}) -> searchRequestId B.==?. B.val_ (getId searchReqID)) $
              B.all_ (BeamCommon.quoteRental BeamCommon.atlasDB)
  pure (either (const 0) (fromMaybe 0) resp)

findById :: MonadFlow m => Id QuoteRental -> m (Maybe QuoteRental)
findById (Id dQuoteId) = findOneWithKV [Se.Is BeamQR.id $ Se.Eq dQuoteId]

instance FromTType' BeamQR.QuoteRental QuoteRental where
  fromTType' BeamQR.QuoteRentalT {..} = do
    fp <- BeamQFP.findById (Id fareParametersId) >>= fromMaybeM (InternalError $ "FareParameters not found in QuoteRental for id: " <> show fareParametersId)
    return $
      Just
        QuoteRental
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

instance ToTType' BeamQR.QuoteRental QuoteRental where
  toTType' QuoteRental {..} = do
    BeamQR.QuoteRentalT
      { BeamQR.id = getId id,
        BeamQR.searchRequestId = getId searchRequestId,
        BeamQR.providerId = getId providerId,
        BeamQR.vehicleVariant = vehicleVariant,
        BeamQR.distance = distance,
        BeamQR.estimatedFinishTime = estimatedFinishTime,
        BeamQR.createdAt = T.utcToLocalTime T.utc createdAt,
        BeamQR.updatedAt = T.utcToLocalTime T.utc updatedAt,
        BeamQR.validTill = T.utcToLocalTime T.utc validTill,
        BeamQR.estimatedFare = estimatedFare,
        BeamQR.specialLocationTag = specialLocationTag,
        BeamQR.fareParametersId = getId fareParams.id
      }
