{-# LANGUAGE TemplateHaskell #-}

module Lib.Payment.Domain.Types.Common where

import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude

data Person

data Merchant

data EntityName = MANUAL | DRIVER_DAILY_STATS | BACKLOG deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnumAndList ''EntityName)

data Ride
