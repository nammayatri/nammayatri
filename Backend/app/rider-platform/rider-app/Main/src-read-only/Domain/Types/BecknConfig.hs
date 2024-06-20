{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.BecknConfig where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Servant.Client.Core
import qualified Tools.Beam.UtilsTH

data BecknConfig = BecknConfig
  { id :: Kernel.Types.Id.Id Domain.Types.BecknConfig.BecknConfig,
    domain :: Kernel.Prelude.Text,
    subscriberId :: Kernel.Prelude.Text,
    subscriberUrl :: Servant.Client.Core.BaseUrl,
    gatewayUrl :: Servant.Client.Core.BaseUrl,
    registryUrl :: Servant.Client.Core.BaseUrl,
    uniqueKeyId :: Kernel.Prelude.Text,
    settlementType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    paymentParamsJson :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleCategory :: Domain.Types.BecknConfig.VehicleCategory,
    collectedBy :: Domain.Types.BecknConfig.PaymentCollectedBy,
    staticTermsUrl :: Kernel.Prelude.Maybe Servant.Client.Core.BaseUrl,
    buyerFinderFee :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    settlementWindow :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    searchTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    selectTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    initTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    confirmTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    confirmBufferTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    bapIFSC :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    statusTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    trackTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    ratingTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    cancelTTLSec :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data PaymentCollectedBy = BAP | BPP deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data VehicleCategory = CAB | AUTO_RICKSHAW | METRO | MOTORCYCLE | AMBULANCE deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''PaymentCollectedBy)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''VehicleCategory)
