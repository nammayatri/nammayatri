{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.BecknConfig where

import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Servant.Client.Core
import qualified Tools.Beam.UtilsTH

data BecknConfig = BecknConfig
  { cancellationFeeAmount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    cancellationFeePercentage :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    collectedBy :: Domain.Types.BecknConfig.PaymentCollectedBy,
    domain :: Kernel.Prelude.Text,
    gatewayUrl :: Servant.Client.Core.BaseUrl,
    id :: Kernel.Types.Id.Id Domain.Types.BecknConfig.BecknConfig,
    paymentParamsJson :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    registryUrl :: Servant.Client.Core.BaseUrl,
    settlementType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    staticTermsUrl :: Kernel.Prelude.Maybe Servant.Client.Core.BaseUrl,
    subscriberId :: Kernel.Prelude.Text,
    subscriberUrl :: Servant.Client.Core.BaseUrl,
    uniqueKeyId :: Kernel.Prelude.Text,
    vehicleCategory :: Domain.Types.BecknConfig.VehicleCategory,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data PaymentCollectedBy = BAP | BPP
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data VehicleCategory = CAB | AUTO_RICKSHAW | METRO
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''PaymentCollectedBy)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''VehicleCategory)
