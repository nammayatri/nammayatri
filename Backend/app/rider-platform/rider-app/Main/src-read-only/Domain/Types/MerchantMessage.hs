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
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data MerchantMessageD (s :: UsageSafety) = MerchantMessage
  { containsUrlButton :: Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime,
    jsonData :: Domain.Types.Extra.MerchantMessage.MerchantMessageDefaultDataJSON,
    language :: Kernel.Prelude.Maybe Kernel.External.Types.Language,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    message :: Kernel.Prelude.Text,
    messageKey :: Domain.Types.MerchantMessage.MessageKey,
    senderHeader :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
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
  | SEND_RIDE_END_OTP
  | SEND_SOS_ALERT
  | MARK_RIDE_AS_SAFE
  | FOLLOW_RIDE
  | ADDED_AS_EMERGENCY_CONTACT
  | TICKET_BOOKING_CANCELLED
  | POST_RIDE_SOS
  | SMS_DELIVERY_DETAILS_SENDER
  | SMS_DELIVERY_DETAILS_RECEIVER
  | POST_DELIVERY_SENDER
  | PRE_PICKUP_DELIVERY_RECEIVER
  | SEND_SCHEDULED_RIDE_DETAILS
  | SCHEDULED_RIDE_OTP
  | WHATSAPP_CALL_BOOKING_FLOW_DETAILS_MESSAGE
  | WHATSAPP_CALL_BOOKING_REALLOCATED_RIDE_DETAILS_MESSAGE
  | WHATSAPP_CALL_BOOKING_CANCELLED_RIDE_MESSAGE
  | METRO_TICKET_BOOKING_CANCELLED
  | METRO_TICKET_BOOKED
  | TICKET_MERCHANT_AGREEMENT_TEMPLATE
  | TICKET_MERCHANT_SNLTR_TEMPLATE
  | PARTNER_ORG_FRFS_TICKET_CANCEL_OTP
  | PASS_PURCHASED_MESSAGE
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

type MerchantMessage = MerchantMessageD 'Safe

instance FromJSON (MerchantMessageD 'Unsafe)

instance ToJSON (MerchantMessageD 'Unsafe)

instance FromJSON (MerchantMessageD 'Safe)

instance ToJSON (MerchantMessageD 'Safe)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''MessageKey)
