{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.CachedQueries.PolicyAndComplianceDocument
  ( findAllLatestEnabledByMerchantAndEntity,
    clearMerchantCache,
  )
where

import qualified Dashboard.Common as Common
import Domain.Types.Merchant (Merchant)
import Domain.Types.PolicyAndComplianceDocument
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.PolicyAndComplianceDocumentExtra as QExtra

findAllLatestEnabledByMerchantAndEntity ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id Merchant ->
  Common.LegalEntityType ->
  m [PolicyAndComplianceDocument]
findAllLatestEnabledByMerchantAndEntity merchantId entityType =
  IM.withInMemCache [key] inMemExpTime $ do
    Hedis.safeGet key >>= \case
      Just docs -> pure docs
      Nothing -> do
        docs <- QExtra.findAllLatestEnabledByMerchantAndEntity merchantId entityType
        expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
        Hedis.setExp key docs expTime
        pure docs
  where
    key = makeKey merchantId entityType

clearMerchantCache :: (CacheFlow m r, MonadFlow m) => Id Merchant -> m ()
clearMerchantCache merchantId =
  forM_ [Common.CustomerLegal, Common.DriverLegal, Common.FleetOwnerLegal] $ \entityType -> do
    let key = makeKey merchantId entityType
    Hedis.runInMultiCloudRedisWrite $ Hedis.del key
    IM.refreshInMem key

makeKey :: Id Merchant -> Common.LegalEntityType -> Text
makeKey merchantId entityType =
  "driver-app:CachedQueries:PolicyDoc:LatestByMerchant-" <> merchantId.getId <> ":" <> show entityType

inMemExpTime :: Seconds
inMemExpTime = 3600
