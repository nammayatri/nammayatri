module Storage.Queries.Merchant where

import Domain.Types.Merchant as DOrg
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
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
