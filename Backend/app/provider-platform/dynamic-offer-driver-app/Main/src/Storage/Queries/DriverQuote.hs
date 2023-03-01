{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.DriverQuote where

import Data.Int (Int32)
import qualified Domain.Types.DriverQuote as Domain
import Domain.Types.Person
import qualified Domain.Types.SearchRequest as DSReq
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (addUTCTime, secondsToNominalDiffTime)
import Storage.Tabular.DriverQuote
import qualified Storage.Tabular.FareParameters as Fare

create :: forall m. Monad m => Domain.DriverQuote -> SqlDB m ()
create dQuote = Esq.runTransaction $
  withFullEntity dQuote $ \(dQuoteT, fareParamsT) -> do
    Esq.create' @Fare.FareParametersT @m fareParamsT
    Esq.create' dQuoteT

baseDriverQuoteQuery ::
  From
    ( SqlExpr (Entity DriverQuoteT)
        :& SqlExpr (Entity Fare.FareParametersT)
    )
baseDriverQuoteQuery =
  table @DriverQuoteT
    `innerJoin` table @Fare.FareParametersT
      `Esq.on` ( \(rb :& farePars) ->
                   rb ^. DriverQuoteFareParametersId ==. farePars ^. Fare.FareParametersTId
               )

findById :: forall m ma. (Transactionable ma m) => Id Domain.DriverQuote -> Proxy ma -> m (Maybe Domain.DriverQuote)
findById dQuoteId _ = buildDType $
  fmap (fmap $ extractSolidType @Domain.DriverQuote) $
    Esq.findOne' @m @ma $ do
      (dQuote :& farePars) <-
        from baseDriverQuoteQuery
      where_ $ dQuote ^. DriverQuoteTId ==. val (toKey dQuoteId)
      pure (dQuote, farePars)

setInactiveByRequestId :: Id DSReq.SearchRequest -> SqlDB m ()
setInactiveByRequestId searchReqId = Esq.update $ \p -> do
  set p [DriverQuoteStatus =. val Domain.Inactive]
  where_ $ p ^. DriverQuoteSearchRequestId ==. val (toKey searchReqId)

findActiveQuotesByDriverId :: forall m ma. (Transactionable ma m, MonadTime m) => Id Person -> Seconds -> Proxy ma -> m [Domain.DriverQuote]
findActiveQuotesByDriverId driverId driverUnlockDelay _ = do
  now <- getCurrentTime
  buildDType $ do
    let delayToAvoidRaces = secondsToNominalDiffTime . negate $ driverUnlockDelay
    fmap (fmap $ extractSolidType @Domain.DriverQuote) $
      Esq.findAll' @m @ma $ do
        (dQuote :& farePars) <-
          from baseDriverQuoteQuery
        where_ $
          dQuote ^. DriverQuoteDriverId ==. val (toKey driverId)
            &&. dQuote ^. DriverQuoteStatus ==. val Domain.Active
            &&. dQuote ^. DriverQuoteValidTill >. val (addUTCTime delayToAvoidRaces now)
        pure (dQuote, farePars)

findAllByRequestId :: forall m ma. Transactionable ma m => Id DSReq.SearchRequest -> Proxy ma -> m [Domain.DriverQuote]
findAllByRequestId searchReqId _ = do
  buildDType $ do
    fmap (fmap $ extractSolidType @Domain.DriverQuote) $
      Esq.findAll' @m @ma $ do
        (dQuote :& farePars) <-
          from baseDriverQuoteQuery
        where_ $
          dQuote ^. DriverQuoteStatus ==. val Domain.Active
            &&. dQuote ^. DriverQuoteSearchRequestId ==. val (toKey searchReqId)
        pure (dQuote, farePars)

countAllByRequestId :: forall m ma. Transactionable ma m => Id DSReq.SearchRequest -> Proxy ma -> m Int32
countAllByRequestId searchReqId _ = do
  fmap (fromMaybe 0) $
    Esq.findOne @m @ma $ do
      dQuote <- from $ table @DriverQuoteT
      where_ $
        dQuote ^. DriverQuoteStatus ==. val Domain.Active
          &&. dQuote ^. DriverQuoteSearchRequestId ==. val (toKey searchReqId)
      pure (countRows @Int32)

deleteByDriverId :: Id Person -> SqlDB m ()
deleteByDriverId personId =
  Esq.delete $ do
    driverQuotes <- from $ table @DriverQuoteT
    where_ $ driverQuotes ^. DriverQuoteDriverId ==. val (toKey personId)
