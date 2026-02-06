{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.DriverWallet where

import Data.Aeson
import Data.OpenApi (ToParamSchema, ToSchema)
import EulerHS.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)

data WalletTransactionType
  = RIDE_TRANSACTION
  | PAYOUT
  | TOPUP
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema)

data RideTransactionType
  = RIDE
  | PLAN_PURCHASE
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkHttpInstancesForEnum ''RideTransactionType)
$(mkHttpInstancesForEnum ''WalletTransactionType)
