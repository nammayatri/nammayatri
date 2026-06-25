{-# LANGUAGE DerivingStrategies #-}

module Dashboard.Common.Rewards.CouponSourceType
  ( CouponSourceType (..),
  )
where

import Data.Aeson
import Kernel.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)

data CouponSourceType = Pool | Templated
  deriving stock (Eq, Show, Generic, Read, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''CouponSourceType)
