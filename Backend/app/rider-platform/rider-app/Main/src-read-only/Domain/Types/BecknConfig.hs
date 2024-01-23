{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.BecknConfig where

import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Servant.Client.Core

data BecknConfig = BecknConfig
  { domain :: Kernel.Prelude.Text,
    gatewayUrl :: Servant.Client.Core.BaseUrl,
    id :: Kernel.Types.Id.Id Domain.Types.BecknConfig.BecknConfig,
    registryUrl :: Servant.Client.Core.BaseUrl,
    subscriberId :: Kernel.Prelude.Text,
    subscriberUrl :: Servant.Client.Core.BaseUrl,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
