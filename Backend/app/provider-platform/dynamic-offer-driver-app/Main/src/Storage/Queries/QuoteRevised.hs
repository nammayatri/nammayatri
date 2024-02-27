{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.QuoteRevised where

import qualified Domain.Types.Common as DTC
import Domain.Types.QuoteRevised
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.QuoteRevised as BeamQR
import Storage.CachedQueries.FarePolicy as BeamFPolicy
import Storage.Queries.FareParameters as BeamQFP
import qualified Storage.Queries.FareParameters as SQFP

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => QuoteRevised -> m ()
create quoteRevised = SQFP.create quoteRevised.fareParams >> createWithKV quoteRevised

findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id QuoteRevised -> m (Maybe QuoteRevised)
findById (Id dQuoteRevisedId) = findOneWithKV [Se.Is BeamQR.id $ Se.Eq dQuoteRevisedId]

instance FromTType' BeamQR.QuoteRevised QuoteRevised where
  fromTType' BeamQR.QuoteRevisedT {..} = do
    farePolicy <- maybe (pure Nothing) (BeamFPolicy.findById . Id) farePolicyId
    fareParams <- BeamQFP.findById (Id fareParametersId) >>= fromMaybeM (InternalError $ "FareParameters not found in Quote for id: " <> show fareParametersId)
    return $
      Just
        QuoteRevised
          { id = Id id,
            searchRequestId = Id searchRequestId,
            providerId = Id providerId,
            tripCategory = fromMaybe (DTC.OneWay DTC.OneWayRideOtp) tripCategory,
            isScheduled = fromMaybe False isScheduled,
            ..
          }

instance ToTType' BeamQR.QuoteRevised QuoteRevised where
  toTType' QuoteRevised {..} = do
    BeamQR.QuoteRevisedT
      { BeamQR.id = getId id,
        BeamQR.searchRequestId = getId searchRequestId,
        BeamQR.providerId = getId providerId,
        BeamQR.vehicleVariant = vehicleVariant,
        BeamQR.estimatedFinishTime = estimatedFinishTime,
        BeamQR.tripCategory = Just tripCategory,
        BeamQR.distance = distance,
        BeamQR.createdAt = createdAt,
        BeamQR.updatedAt = updatedAt,
        BeamQR.validTill = validTill,
        BeamQR.estimatedFare = estimatedFare,
        BeamQR.specialLocationTag = specialLocationTag,
        BeamQR.fareParametersId = getId fareParams.id,
        BeamQR.isScheduled = Just isScheduled,
        BeamQR.farePolicyId = (getId . (.id)) <$> farePolicy
      }
