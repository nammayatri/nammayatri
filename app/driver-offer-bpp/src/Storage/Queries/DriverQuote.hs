module Storage.Queries.DriverQuote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Common (addUTCTime, secondsToNominalDiffTime)
import Data.Int (Int32)
import qualified Domain.Types.DriverQuote as Domain
import Domain.Types.Person
import qualified Domain.Types.SearchRequest as DSReq
import Storage.Tabular.DriverQuote
import qualified Storage.Tabular.FareParameters as Fare

create :: Domain.DriverQuote -> SqlDB ()
create dQuote = Esq.runTransaction $
  withFullEntity dQuote $ \(dQuoteT, fareParamsT) -> do
    Esq.create' fareParamsT
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

findById :: (Transactionable m) => Id Domain.DriverQuote -> m (Maybe Domain.DriverQuote)
findById dQuoteId = buildDType $
  fmap (fmap $ extractSolidType @Domain.DriverQuote) $
    Esq.findOne' $ do
      (dQuote :& farePars) <-
        from baseDriverQuoteQuery
      where_ $ dQuote ^. DriverQuoteTId ==. val (toKey dQuoteId)
      pure (dQuote, farePars)

setInactiveByRequestId :: Id DSReq.SearchRequest -> SqlDB ()
setInactiveByRequestId searchReqId = Esq.update $ \p -> do
  set p [DriverQuoteStatus =. val Domain.Inactive]
  where_ $ p ^. DriverQuoteSearchRequestId ==. val (toKey searchReqId)

findActiveQuotesByDriverId :: (Transactionable m, MonadTime m) => Id Person -> Seconds -> m [Domain.DriverQuote]
findActiveQuotesByDriverId driverId driverUnlockDelay = do
  now <- getCurrentTime
  buildDType $ do
    let delayToAvoidRaces = secondsToNominalDiffTime . negate $ driverUnlockDelay
    fmap (fmap $ extractSolidType @Domain.DriverQuote) $
      Esq.findAll' $ do
        (dQuote :& farePars) <-
          from baseDriverQuoteQuery
        where_ $
          dQuote ^. DriverQuoteDriverId ==. val (toKey driverId)
            &&. dQuote ^. DriverQuoteStatus ==. val Domain.Active
            &&. dQuote ^. DriverQuoteValidTill >. val (addUTCTime delayToAvoidRaces now)
        pure (dQuote, farePars)

findAllByRequestId :: Transactionable m => Id DSReq.SearchRequest -> m [Domain.DriverQuote]
findAllByRequestId searchReqId = do
  buildDType $ do
    fmap (fmap $ extractSolidType @Domain.DriverQuote) $
      Esq.findAll' $ do
        (dQuote :& farePars) <-
          from baseDriverQuoteQuery
        where_ $
          dQuote ^. DriverQuoteStatus ==. val Domain.Active
            &&. dQuote ^. DriverQuoteSearchRequestId ==. val (toKey searchReqId)
        pure (dQuote, farePars)

countAllByRequestId :: Transactionable m => Id DSReq.SearchRequest -> m Int32
countAllByRequestId searchReqId = do
  fmap (fromMaybe 0) $
    Esq.findOne $ do
      dQuote <- from $ table @DriverQuoteT
      where_ $
        dQuote ^. DriverQuoteStatus ==. val Domain.Active
          &&. dQuote ^. DriverQuoteSearchRequestId ==. val (toKey searchReqId)
      pure (countRows @Int32)

deleteByDriverId :: Id Person -> SqlDB ()
deleteByDriverId personId =
  Esq.delete $ do
    driverQuotes <- from $ table @DriverQuoteT
    where_ $ driverQuotes ^. DriverQuoteDriverId ==. val (toKey personId)
