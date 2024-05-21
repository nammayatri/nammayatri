{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.Message where

import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Message
import qualified Domain.Types.Message
import Domain.Types.MessageTranslation as DomainMT
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.MessageTranslation as MT

getMerchantOperatingCityId :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => (Text -> m (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity))
getMerchantOperatingCityId merchantId = do
  merchant <- CQM.findById (Id merchantId) >>= fromMaybeM (MerchantNotFound merchantId)
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant Nothing
  pure merchantOpCityId

getMsgTranslations :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m [Domain.Types.Message.MessageTranslation]
getMsgTranslations id = do
  mT' <- MT.findByMessageId (Id id)
  let mT = (\(DomainMT.MessageTranslation createdAt_ description_ label_ language_ _ shortDescription_ title_) -> Domain.Types.Message.MessageTranslation createdAt_ description_ label_ language_ shortDescription_ title_) <$> mT'
  return mT
