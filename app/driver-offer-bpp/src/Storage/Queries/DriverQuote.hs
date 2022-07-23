module Storage.Queries.DriverQuote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Common (HasPrettyLogger, logPretty)
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
  fmap (fmap extractSolidType) $
    Esq.findOne' $ do
      (dQuote :& farePars) <-
        from baseDriverQuoteQuery
      where_ $ dQuote ^. DriverQuoteTId ==. val (toKey dQuoteId)
      pure (dQuote, farePars)

setInactiveByRequestId :: Id DSReq.SearchRequest -> SqlDB ()
setInactiveByRequestId searchReqId = Esq.update $ \p -> do
  set p [DriverQuoteStatus =. val Domain.Inactive]
  where_ $ p ^. DriverQuoteSearchRequestId ==. val (toKey searchReqId)

findActiveQuotesByDriverId :: (Transactionable m, MonadTime m) => Id Person -> m [Domain.DriverQuote]
findActiveQuotesByDriverId driverId = (getCurrentTime >>=) $ \now -> buildDType $
  fmap (fmap extractSolidType) $
    Esq.findAll' $ do
      (dQuote :& farePars) <-
        from baseDriverQuoteQuery
      where_ $
        dQuote ^. DriverQuoteDriverId ==. val (toKey driverId)
          &&. dQuote ^. DriverQuoteStatus ==. val Domain.Active
          &&. dQuote ^. DriverQuoteValidTill >. val now
      pure (dQuote, farePars)

thereAreActiveQuotes :: (Transactionable m, MonadTime m, HasPrettyLogger m r) => Id Person -> m Bool
thereAreActiveQuotes driverId = do
  activeQuotes <- findActiveQuotesByDriverId driverId
  logPretty DEBUG ("active quotes for driverId = " <> driverId.getId) activeQuotes
  pure $ not $ null activeQuotes
