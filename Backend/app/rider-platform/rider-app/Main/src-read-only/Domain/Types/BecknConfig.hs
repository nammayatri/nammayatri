{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.BecknConfig where

import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Servant.Client.Core
import qualified Tools.Beam.UtilsTH

data BecknConfig = BecknConfig
  { domain :: Kernel.Prelude.Text,
    gatewayUrl :: Servant.Client.Core.BaseUrl,
    id :: Kernel.Types.Id.Id Domain.Types.BecknConfig.BecknConfig,
    paymentParamsJson :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    registryUrl :: Servant.Client.Core.BaseUrl,
    settlementType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    subscriberId :: Kernel.Prelude.Text,
    subscriberUrl :: Servant.Client.Core.BaseUrl,
    uniqueKeyId :: Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data BknPaymentParams = BknPaymentParams
  { bankAccNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    bankCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vpa :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
