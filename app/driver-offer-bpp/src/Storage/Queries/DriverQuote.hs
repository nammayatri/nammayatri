module Storage.Queries.DriverQuote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import qualified Domain.Types.DriverQuote as Domain
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
