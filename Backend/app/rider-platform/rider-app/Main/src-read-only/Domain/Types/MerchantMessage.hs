{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MerchantMessage (module Domain.Types.MerchantMessage, module ReExport) where

import Data.Aeson
import Domain.Types.Common (UsageSafety (..))
import Domain.Types.Extra.MerchantMessage as ReExport
import qualified Domain.Types.Extra.MerchantMessage
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data MerchantMessageD (s :: UsageSafety) = MerchantMessage
  { merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    messageKey :: Domain.Types.MerchantMessage.MessageKey,
    message :: Kernel.Prelude.Text,
    templateId :: Kernel.Prelude.Text,
    jsonData :: Domain.Types.Extra.MerchantMessage.MerchantMessageDefaultDataJSON,
    containsUrlButton :: Kernel.Prelude.Bool,
    updatedAt :: Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

data MessageKey
  = INVITE_TO_UNEXISTENT_EMERGENCY_NUMBER
  | SET_AS_RIDE_EMERGENCY_NUMBER
  | SET_AS_DEFAULT_EMERGENCY_NUMBER
  | SEND_OTP
  | SEND_BOOKING_OTP
  | SEND_SOS_ALERT
  | MARK_RIDE_AS_SAFE
  | FOLLOW_RIDE
  | ADDED_AS_EMERGENCY_CONTACT
  | TICKET_BOOKING_CANCELLED
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

type MerchantMessage = MerchantMessageD 'Safe

instance FromJSON (MerchantMessageD 'Unsafe)

instance ToJSON (MerchantMessageD 'Unsafe)

instance FromJSON (MerchantMessageD 'Safe)

instance ToJSON (MerchantMessageD 'Safe)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''MessageKey)
