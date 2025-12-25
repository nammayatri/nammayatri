module Domain.Action.UI.MerchantServiceConfig where

import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.MerchantServiceConfig
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id

buildMerchantServiceConfig ::
  MonadTime m =>
  Id Merchant ->
  ServiceConfig ->
  Id DMOC.MerchantOperatingCity ->
  m MerchantServiceConfig
buildMerchantServiceConfig merchantId serviceConfig merchantOperatingCityId = do
  now <- getCurrentTime
  pure
    MerchantServiceConfig
      { merchantId,
        serviceConfig,
        merchantOperatingCityId = Just merchantOperatingCityId,
        updatedAt = now,
        createdAt = now
      }
