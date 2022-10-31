module Storage.Queries.Merchant where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Merchant as DOrg
import Storage.Tabular.Merchant

findById ::
  Transactionable m =>
  Id Merchant ->
  m (Maybe Merchant)
findById = Esq.findById

findByShortId :: Transactionable m => ShortId Merchant -> m (Maybe Merchant)
findByShortId shortId = do
  findOne $ do
    merchant <- from $ table @MerchantT
    where_ $ merchant ^. MerchantShortId ==. val (getShortId shortId)
    return merchant
