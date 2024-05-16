{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.Person where

import qualified Domain.Types.Merchant
import Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Merchant.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

getMerchantOpCId :: KvDbFlow m r => Text -> Maybe Text -> m (Kernel.Types.Id.Id MerchantOperatingCity)
getMerchantOpCId merchantId merchantOperatingCityId = do
  merchant <- CQM.findById (Kernel.Types.Id.Id merchantId) >>= fromMaybeM (MerchantNotFound merchantId)
  CQMOC.getMerchantOpCityId (Kernel.Types.Id.Id <$> merchantOperatingCityId) merchant Nothing
