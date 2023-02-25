module Storage.Queries.TransporterConfig
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Merchant
import Domain.Types.TransporterConfig
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.TransporterConfig

findByMerchantId :: Transactionable m => Id Merchant -> m (Maybe TransporterConfig)
findByMerchantId merchantId =
  Esq.findOne $ do
    config <- from $ table @TransporterConfigT
    where_ $
      config ^. TransporterConfigMerchantId ==. val (toKey merchantId)
    return config

updateFCMConfig :: Id Merchant -> BaseUrl -> Text -> SqlDB ()
updateFCMConfig merchantId fcmUrl fcmServiceAccount = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ TransporterConfigFcmUrl =. val (showBaseUrl fcmUrl),
        TransporterConfigFcmServiceAccount =. val fcmServiceAccount,
        TransporterConfigUpdatedAt =. val now
      ]
    where_ $ tbl ^. TransporterConfigMerchantId ==. val (toKey merchantId)


updateReferralLinkPassword :: Id Merchant -> Maybe Text -> SqlDB ()
updateReferralLinkPassword merchantId newPassword =  do
    Esq.update $ \tbl -> do
      set
        tbl
        [
          TransporterConfigReferralLinkPassword =. val newPassword
        ]
      where_ $ tbl ^. TransporterConfigMerchantId ==. val (toKey merchantId)


