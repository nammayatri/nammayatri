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
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Storage.Tabular.FareParameters as Fare
import Storage.Tabular.QuoteSpecialZone

create :: QuoteSpecialZone -> SqlDB ()
create quote = Esq.runTransaction $
  withFullEntity quote $ \(quoteT, fareParamsT) -> do
    Esq.create' fareParamsT
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
findById dQuoteId = buildDType $
  fmap (fmap $ extractSolidType @QuoteSpecialZone) $
    Esq.findOne' $ do
      (dQuote :& farePars) <-
        from baseQuoteSpecialZoneQuery
      where_ $ dQuote ^. QuoteSpecialZoneTId ==. val (toKey dQuoteId)
      pure (dQuote, farePars)
