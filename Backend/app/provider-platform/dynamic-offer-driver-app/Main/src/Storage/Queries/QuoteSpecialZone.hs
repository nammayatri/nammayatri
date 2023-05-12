{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.QuoteSpecialZone where

import Data.Int (Int32)
import Domain.Types.QuoteSpecialZone
import Domain.Types.SearchRequestSpecialZone
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Queries.FullEntityBuilders (buildFullQuoteSpecialZone)
import qualified Lib.Mesh as Mesh
import Lib.Utils
import qualified Sequelize as Se
import Storage.Beam.FareParameters as BeamFP hiding (Id)
import qualified Storage.Beam.QuoteSpecialZone as BeamQSZ
import Storage.Queries.FullEntityBuilders (buildFullQuoteSpecialZone)
import Storage.Queries.FareParameters as BeamQFP
import Storage.Queries.FullEntityBuilders (buildFullQuoteSpecialZone)
import qualified Storage.Tabular.FareParameters as Fare
import qualified Storage.Tabular.FareParameters.Instances as FareParamsT
import Storage.Tabular.QuoteSpecialZone
import qualified Storage.Tabular.VechileNew as VN

create :: QuoteSpecialZone -> SqlDB ()
create quote = Esq.runTransaction $
  withFullEntity quote $ \(quoteT, (fareParams', fareParamsDetais)) -> do
    Esq.create' fareParams'
    case fareParamsDetais of
      FareParamsT.ProgressiveDetailsT fppdt -> Esq.create' fppdt
      FareParamsT.SlabDetailsT -> return ()
    Esq.create' quoteT

countAllByRequestId :: Transactionable m => Id SearchRequestSpecialZone -> m Int32
countAllByRequestId searchReqId = do
  fmap (fromMaybe 0) $
    Esq.findOne $ do
      dQuote <- from $ table @QuoteSpecialZoneT
      where_ $
        dQuote ^. QuoteSpecialZoneSearchRequestId ==. val (toKey searchReqId)
      pure (countRows @Int32)

baseQuoteSpecialZoneQuery ::
  From
    ( SqlExpr (Entity QuoteSpecialZoneT)
        :& SqlExpr (Entity Fare.FareParametersT)
    )
baseQuoteSpecialZoneQuery =
  table @QuoteSpecialZoneT
    `innerJoin` table @Fare.FareParametersT
      `Esq.on` ( \(rb :& farePars) ->
                   rb ^. QuoteSpecialZoneFareParametersId ==. farePars ^. Fare.FareParametersTId
               )

findById :: (Transactionable m) => Id QuoteSpecialZone -> m (Maybe QuoteSpecialZone)
findById dQuoteId = buildDType $ do
  res <- Esq.findOne' $ do
    (dQuote :& farePars) <-
      from baseQuoteSpecialZoneQuery
    where_ $ dQuote ^. QuoteSpecialZoneTId ==. val (toKey dQuoteId)
    pure (dQuote, farePars)
  join <$> mapM buildFullQuoteSpecialZone res

-- transformBeamQuoteSpecialZoneToDomain :: BeamQSZ.QuoteSpecialZone -> QuoteSpecialZone
-- transformBeamQuoteSpecialZoneToDomain BeamQSZ.QuoteSpecialZoneT {..} = do
--   QuoteSpecialZone
--     {
--       id = Id id,
--       searchRequestId = Id searchRequestId,
--       providerId = Id providerId,
--       vehicleVariant = vehicleVariant,
--       distance = distance,
--       estimatedFinishTime = estimatedFinishTime,
--       createdAt = createdAt,
--       updatedAt = updatedAt,
--       validTill = validTill,
--       estimatedFare = estimatedFare,
--       fareParams = fareParams
--     }

-- transformDomainQuoteSpecialZoneToBeam :: QuoteSpecialZone -> BeamQSZ.QuoteSpecialZone
-- transformDomainQuoteSpecialZoneToBeam QuoteSpecialZone {..} =
--   BeamQSZ.defaultQuoteSpecialZone
--     {
--       BeamQSZ.id = getId id,
--       BeamQSZ.searchRequestId = getId searchRequestId,
--       BeamQSZ.providerId = getId providerId,
--       BeamQSZ.vehicleVariant = vehicleVariant,
--       BeamQSZ.distance = distance,
--       BeamQSZ.estimatedFinishTime = estimatedFinishTime,
--       BeamQSZ.createdAt = createdAt,
--       BeamQSZ.updatedAt = updatedAt,
--       BeamQSZ.validTill = validTill,
--       BeamQSZ.estimatedFare = estimatedFare,
--       BeamQSZ.fareParams = fareParams
--     }
