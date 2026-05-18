{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.IntegratedBPPConfig where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified Kernel.Types.Time
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data IntegratedBPPConfig = IntegratedBPPConfig
  { agencyKey :: Kernel.Prelude.Text,
    city :: Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City,
    domain :: Kernel.Prelude.Text,
    feedKey :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    platformType :: Domain.Types.IntegratedBPPConfig.PlatformType,
    providerConfig :: Domain.Types.IntegratedBPPConfig.ProviderConfig,
    vehicleCategory :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data DIRECTConfig = DIRECTConfig {cipherKey :: Kernel.Prelude.Text, qrRefreshTtl :: Kernel.Prelude.Maybe Kernel.Types.Time.Seconds} deriving (Generic, FromJSON, ToJSON, Eq, Show)

data ONDCBecknConfig = ONDCBecknConfig
  { fareCachingAllowed :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    multiInitAllowed :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    networkHostUrl :: Kernel.Prelude.Maybe Kernel.Prelude.BaseUrl,
    networkId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    overrideCity :: Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City,
    redisPrefix :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    singleTicketForMultiplePassengers :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving (Generic, FromJSON, ToJSON, Eq, Show)

data PlatformType = MULTIMODAL | PARTNERORG | APPLICATION deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data ProviderConfig = DIRECT Domain.Types.IntegratedBPPConfig.DIRECTConfig | ONDC Domain.Types.IntegratedBPPConfig.ONDCBecknConfig deriving (Generic, FromJSON, ToJSON, Eq, Show)

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum ''PlatformType)

$(Kernel.Utils.TH.mkToHttpInstanceForEnum ''PlatformType)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''PlatformType)
