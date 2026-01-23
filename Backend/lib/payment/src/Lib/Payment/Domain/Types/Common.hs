module Lib.Payment.Domain.Types.Common where

import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude

data Person

data Merchant

data MerchantOperatingCity

data EntityName = MANUAL | DRIVER_DAILY_STATS | BACKLOG | DAILY_STATS_VIA_DASHBOARD | RETRY_VIA_DASHBOARD | DRIVER_FEE | METRO_BOOKING_CASHBACK | REFERRAL_AWARD_RIDE | REFERRED_BY_AWARD | REFERRED_BY_AND_BACKLOG_AWARD | DRIVER_WALLET_TRANSACTION | DRIVER_WALLET_TOPUP | SPECIAL_ZONE_PAYOUT deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnumAndList ''EntityName)

data Ride

data PaymentFulfillmentStatus
  = FulfillmentPending
  | FulfillmentFailed
  | FulfillmentSucceeded
  | FulfillmentRefundPending
  | FulfillmentRefundInitiated
  | FulfillmentRefundFailed
  | FulfillmentRefunded
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON, Generic, ToSchema, ToParamSchema)

$(mkBeamInstancesForEnum ''PaymentFulfillmentStatus)
