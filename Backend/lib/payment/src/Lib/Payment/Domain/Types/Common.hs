{-# LANGUAGE TemplateHaskell #-}

module Lib.Payment.Domain.Types.Common where

import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude

data Person

data Merchant

data MerchantOperatingCity

data EntityName = MANUAL | DRIVER_DAILY_STATS | BACKLOG | DAILY_STATS_VIA_DASHBOARD | RETRY_VIA_DASHBOARD | DRIVER_FEE | METRO_BOOKING_CASHBACK deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnumAndList ''EntityName)

data Ride
