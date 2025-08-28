{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}

module Domain.Types.Extra.PartnerOrgConfig where

import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HMS
import EulerHS.Prelude ((+||), (||+))
import Kernel.Beam.Lib.UtilsTH (mkBeamInstancesForEnumAndList)
import Kernel.Prelude
import Kernel.Sms.Config (SmsSessionConfig)
import Kernel.Types.SlidingWindowLimiter (APIRateLimitOptions)
import Kernel.Utils.Common
import Kernel.Utils.GenericPretty
import Tools.Error

-- Extra code goes here --

data ConfigType
  = REGISTRATION
  | RATE_LIMIT
  | TICKET_SMS
  | BPP_STATUS_CALL
  | WALLET_CLASS_NAME
  | WALLET_QR_TYPE
  deriving (Generic, Eq, Ord, Read, Show, ToSchema, ToParamSchema, ToJSON, FromJSON)
  deriving (PrettyShow) via Showable ConfigType

$(mkBeamInstancesForEnumAndList ''ConfigType)

data PartnerOrganizationConfig
  = Registration RegistrationConfig
  | RateLimit RateLimitConfig
  | TicketSMS TicketSMSConfig
  | BPPStatusCall BPPStatusCallConfig
  | WalletClassName WalletClassNameConfig
  | WalletQRType WalletQRTypeConfig
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data RegistrationConfig = RegistrationConfig
  { fakeOtp :: Text,
    sessionConfig :: SmsSessionConfig
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

newtype RateLimitConfig = RateLimitConfig
  { rateLimitOptions :: APIRateLimitOptions
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

newtype WalletClassNameConfig = WalletClassNameConfig
  { className :: HMS.HashMap Text Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

newtype WalletQRTypeConfig = WalletQRTypeConfig
  { qrType :: HMS.HashMap Text Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data TicketSMSConfig = TicketSMSConfig
  { -- | Template for public url have variable:
    -- | 1. `{#FRFS_BOOKING_ID#}` as a placeholder for the booking id
    publicUrl :: Maybe Text,
    shortUrlExpiryInHours :: Maybe Int
  }
  deriving (Generic, Show, ToSchema)

instance FromJSON TicketSMSConfig where
  parseJSON = genericParseJSON optionsTicketSMSConfig

instance ToJSON TicketSMSConfig where
  toJSON = genericToJSON optionsTicketSMSConfig

optionsTicketSMSConfig :: A.Options
optionsTicketSMSConfig = defaultOptions {omitNothingFields = True}

newtype BPPStatusCallConfig = BPPStatusCallConfig
  { intervalInSec :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

getConfigType :: PartnerOrganizationConfig -> ConfigType
getConfigType (Registration _) = REGISTRATION
getConfigType (RateLimit _) = RATE_LIMIT
getConfigType (TicketSMS _) = TICKET_SMS
getConfigType (BPPStatusCall _) = BPP_STATUS_CALL
getConfigType (WalletClassName _) = WALLET_CLASS_NAME
getConfigType (WalletQRType _) = WALLET_QR_TYPE

getConfigJSON :: PartnerOrganizationConfig -> A.Value
getConfigJSON (Registration cfg) = A.toJSON cfg
getConfigJSON (RateLimit cfg) = A.toJSON cfg
getConfigJSON (TicketSMS cfg) = A.toJSON cfg
getConfigJSON (BPPStatusCall cfg) = A.toJSON cfg
getConfigJSON (WalletClassName cfg) = A.toJSON cfg
getConfigJSON (WalletQRType cfg) = A.toJSON cfg

getRegistrationConfig :: (MonadFlow m) => PartnerOrganizationConfig -> m RegistrationConfig
getRegistrationConfig (Registration cfg) = pure cfg
getRegistrationConfig cfg = throwError . InternalError $ unknownConfigType REGISTRATION cfg

getRateLimitConfig :: (MonadFlow m) => PartnerOrganizationConfig -> m RateLimitConfig
getRateLimitConfig (RateLimit cfg) = pure cfg
getRateLimitConfig cfg = throwError . InternalError $ unknownConfigType RATE_LIMIT cfg

getTicketSMSConfig :: (MonadFlow m) => PartnerOrganizationConfig -> m TicketSMSConfig
getTicketSMSConfig (TicketSMS cfg) = pure cfg
getTicketSMSConfig cfg = throwError . InternalError $ unknownConfigType TICKET_SMS cfg

getBPPStatusCallConfig :: (MonadFlow m) => PartnerOrganizationConfig -> m BPPStatusCallConfig
getBPPStatusCallConfig (BPPStatusCall cfg) = pure cfg
getBPPStatusCallConfig cfg = throwError . InternalError $ unknownConfigType BPP_STATUS_CALL cfg

getWalletClassNameConfig :: (MonadFlow m) => PartnerOrganizationConfig -> m WalletClassNameConfig
getWalletClassNameConfig (WalletClassName cfg) = pure cfg
getWalletClassNameConfig cfg = throwError . InternalError $ unknownConfigType WALLET_CLASS_NAME cfg

getWalletQRTypeConfig :: (MonadFlow m) => PartnerOrganizationConfig -> m WalletQRTypeConfig
getWalletQRTypeConfig (WalletQRType cfg) = pure cfg
getWalletQRTypeConfig cfg = throwError . InternalError $ unknownConfigType WALLET_QR_TYPE cfg

unknownConfigType :: ConfigType -> PartnerOrganizationConfig -> Text
unknownConfigType cfgType cfg = "Unknown Partner Org Config type, expected:" +|| cfgType ||+ " but got:" +|| cfg ||+ ""
