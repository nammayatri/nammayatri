{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MerchantMessage (module Domain.Types.MerchantMessage, module ReExport) where

import Domain.Types.Common (UsageSafety (..))
import Domain.Types.Extra.MerchantMessage as ReExport
import qualified Domain.Types.Extra.MerchantMessage
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data MerchantMessageD (s :: UsageSafety) = MerchantMessage
  { containsUrlButton :: Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime,
    jsonData :: Domain.Types.Extra.MerchantMessage.MerchantMessageDefaultDataJSON,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    message :: Kernel.Prelude.Text,
    messageKey :: Domain.Types.MerchantMessage.MessageKey,
    templateId :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
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
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

type MerchantMessage = MerchantMessageD ('Safe)

instance FromJSON (MerchantMessageD 'Unsafe)

instance ToJSON (MerchantMessageD 'Unsafe)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''MessageKey))
