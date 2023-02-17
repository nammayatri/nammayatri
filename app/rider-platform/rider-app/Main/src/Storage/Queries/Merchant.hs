{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Merchant
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Merchant as DOrg
import Kernel.Prelude
import Kernel.Storage.Esqueleto hiding (findById)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
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

findAll :: Transactionable m => m [Merchant]
findAll =
  Esq.findAll $ do from $ table @MerchantT

update :: Merchant -> SqlDB ()
update merchant = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ MerchantName =. val merchant.name,
        MerchantFcmUrl =. val (showBaseUrl merchant.fcmConfig.fcmUrl),
        MerchantFcmServiceAccount =. val merchant.fcmConfig.fcmServiceAccount,
        MerchantExoPhone =. val merchant.exoPhone,
        MerchantExoPhoneCountryCode =. val merchant.exoPhoneCountryCode,
        MerchantGatewayUrl =. val (showBaseUrl merchant.gatewayUrl),
        MerchantRegistryUrl =. val (showBaseUrl merchant.registryUrl),
        MerchantUpdatedAt =. val now
      ]
    where_ $ tbl ^. MerchantTId ==. val (toKey merchant.id)
