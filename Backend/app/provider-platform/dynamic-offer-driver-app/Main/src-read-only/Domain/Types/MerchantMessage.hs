{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MerchantMessage (module Domain.Types.MerchantMessage, module ReExport) where

import Data.Aeson
import Domain.Types.Common (UsageSafety (..))
import Domain.Types.Extra.MerchantMessage as ReExport
import qualified Domain.Types.Extra.MerchantMessage
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.VehicleCategory
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data MerchantMessageD (s :: UsageSafety) = MerchantMessage
  { channel :: Kernel.Prelude.Maybe Domain.Types.MerchantMessage.MediaChannel,
    containsUrlButton :: Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime,
    domain :: Kernel.Prelude.Maybe Domain.Types.MerchantMessage.MessageDomain,
    jsonData :: Domain.Types.Extra.MerchantMessage.MerchantMessageDefaultDataJSON,
    mediaUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    message :: Kernel.Prelude.Text,
    messageKey :: Domain.Types.Extra.MerchantMessage.MessageKey,
    messageType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    senderHeader :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    templateId :: Kernel.Prelude.Text,
    templateName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory
  }
  deriving (Generic, Show, Eq)

data MediaChannel = SMS | WHATSAPP | OVERLAY | ALERT deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, Bounded, Enum)

data MessageDomain = FLEET | RIDE_HAILING | GENERAL deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, Bounded, Enum)

type MerchantMessage = MerchantMessageD 'Safe

instance FromJSON (MerchantMessageD 'Unsafe)

instance ToJSON (MerchantMessageD 'Unsafe)

instance FromJSON (MerchantMessageD 'Safe)

instance ToJSON (MerchantMessageD 'Safe)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''MediaChannel)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''MessageDomain)
