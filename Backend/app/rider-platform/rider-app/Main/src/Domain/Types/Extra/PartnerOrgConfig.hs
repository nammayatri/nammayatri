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
  | ACCOUNT_LINK
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
  | AccountLink AccountLinkConfig
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

-- | How a partner (e.g. Ouru) links a rider's existing or new account: the rider enters
-- an OTP on our hosted page and the token goes to the partner's webhook. The webhook
-- URL is fixed here, never taken from a request, so a leaked partner code cannot send
-- a token anywhere else.
data AccountLinkConfig = AccountLinkConfig
  { -- | Where {request_id, access_token} is POSTed, signed with HMAC-SHA256.
    webhookUrl :: Text,
    -- | Our own public base URL for the hosted page, e.g. https://api.moving.tech/pilot/app/v2
    pageBaseUrl :: Text,
    -- | How long a link stays usable, start to finish.
    linkExpiryInSec :: Int,
    -- | Lifetime of the partner token, in days (same unit as RegistrationToken.tokenExpiry).
    tokenExpiryInDays :: Int,
    -- | Wrong OTPs allowed before the link dies.
    maxOtpAttempts :: Int,
    -- | OTP sends (first + resends) allowed per link.
    maxOtpSends :: Int,
    -- | Shown on the consent page: "<partnerDisplayName> wants to book rides for you".
    partnerDisplayName :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

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
getConfigType (AccountLink _) = ACCOUNT_LINK

getConfigJSON :: PartnerOrganizationConfig -> A.Value
getConfigJSON (Registration cfg) = A.toJSON cfg
getConfigJSON (RateLimit cfg) = A.toJSON cfg
getConfigJSON (TicketSMS cfg) = A.toJSON cfg
getConfigJSON (BPPStatusCall cfg) = A.toJSON cfg
getConfigJSON (WalletClassName cfg) = A.toJSON cfg
getConfigJSON (WalletQRType cfg) = A.toJSON cfg
getConfigJSON (AccountLink cfg) = A.toJSON cfg

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

getAccountLinkConfig :: (MonadFlow m) => PartnerOrganizationConfig -> m AccountLinkConfig
getAccountLinkConfig (AccountLink cfg) = pure cfg
getAccountLinkConfig cfg = throwError . InternalError $ unknownConfigType ACCOUNT_LINK cfg
