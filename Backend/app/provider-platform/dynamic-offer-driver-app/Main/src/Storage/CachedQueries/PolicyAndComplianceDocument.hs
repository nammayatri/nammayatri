{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.CachedQueries.PolicyAndComplianceDocument
  ( findAllLatestEnabledByMerchant,
    clearMerchantCache,
  )
where

import Domain.Types.Merchant (Merchant)
import Domain.Types.PolicyAndComplianceDocument
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.PolicyAndComplianceDocumentExtra as QExtra

findAllLatestEnabledByMerchant ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id Merchant ->
  m [PolicyAndComplianceDocument]
findAllLatestEnabledByMerchant merchantId =
  IM.withInMemCache [key] inMemExpTime $ do
    Hedis.safeGet key >>= \case
      Just docs -> pure docs
      Nothing -> do
        docs <- QExtra.findAllLatestEnabledByMerchant merchantId
        expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
        Hedis.setExp key docs expTime
        pure docs
  where
    key = makeKey merchantId

clearMerchantCache :: (CacheFlow m r, MonadFlow m) => Id Merchant -> m ()
clearMerchantCache merchantId = do
  let key = makeKey merchantId
  Hedis.runInMultiCloudRedisWrite $ Hedis.del key
  IM.refreshInMem key

makeKey :: Id Merchant -> Text
makeKey merchantId = "driver-app:CachedQueries:PolicyDoc:LatestByMerchant-" <> merchantId.getId

inMemExpTime :: Seconds
inMemExpTime = 3600
