{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Merchant where

import Beckn.External.Encryption
import Beckn.Prelude
import Beckn.Storage.Esqueleto hiding (findById)
import qualified Beckn.Storage.Esqueleto as Esq (findById)
import Beckn.Types.App (MonadFlow)
import Beckn.Types.Id
import Beckn.Utils.Common (fromMaybeM)
import Domain.Types.Merchant as DOrg
import Storage.Tabular.Merchant
import Types.Error (MerchantError (MerchantNotFound))

findById :: Transactionable m => Id Merchant -> m (Maybe Merchant)
findById = Esq.findById

findByShortId :: Transactionable m => ShortId Merchant -> m (Maybe Merchant)
findByShortId shortId_ = do
  findOne $ do
    merchant <- from $ table @MerchantT
    where_ $ merchant ^. MerchantShortId ==. val (getShortId shortId_)
    return merchant

findByExoPhone :: (Transactionable m, EncFlow m r) => Text -> Text -> m (Maybe Merchant)
findByExoPhone countryCode exoPhone = do
  findOne $ do
    merchant <- from $ table @MerchantT
    where_ $
      merchant ^. MerchantExoPhoneCountryCode ==. val (Just countryCode)
        &&. merchant ^. MerchantExoPhone ==. val (Just exoPhone)
    return merchant

-- utils functions
withMerchantConfig ::
  (MonadFlow m, Transactionable m) =>
  (Merchant -> a) ->
  Id Merchant ->
  m a
withMerchantConfig func merchId = do
  mbFullConfig <- findById merchId
  full <- mbFullConfig & fromMaybeM (MerchantNotFound merchId.getId)
  pure $ func full
