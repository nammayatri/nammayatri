{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Merchant
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto hiding (findById)
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Merchant as DOrg
import Storage.Tabular.Merchant

findById :: Transactionable m => Id Merchant -> m (Maybe Merchant)
findById = Esq.findById

findByShortId :: Transactionable m => ShortId Merchant -> m (Maybe Merchant)
findByShortId shortId_ = do
  findOne $ do
    merchant <- from $ table @MerchantT
    where_ $ merchant ^. MerchantShortId ==. val (getShortId shortId_)
    return merchant

findByExoPhone :: Transactionable m => Text -> Text -> m (Maybe Merchant)
findByExoPhone countryCode exoPhone = do
  findOne $ do
    merchant <- from $ table @MerchantT
    where_ $
      merchant ^. MerchantExoPhoneCountryCode ==. val (Just countryCode)
        &&. merchant ^. MerchantExoPhone ==. val (Just exoPhone)
    return merchant
