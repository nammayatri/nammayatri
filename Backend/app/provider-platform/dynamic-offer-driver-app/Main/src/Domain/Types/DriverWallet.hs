{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.DriverWallet where

import Data.Aeson
import Data.OpenApi (ToParamSchema, ToSchema)
import GHC.Generics (Generic)
import Kernel.Prelude (Eq, Read, Show)
import Kernel.Utils.TH (mkHttpInstancesForEnum)

data RideTransactionType
  = RIDE
  | PLAN_PURCHASE
  | SUBSCRIPTION_EXPIRY
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkHttpInstancesForEnum ''RideTransactionType)
