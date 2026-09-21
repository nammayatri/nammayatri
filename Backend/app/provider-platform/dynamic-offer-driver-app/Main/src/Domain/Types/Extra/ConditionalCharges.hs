{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Extra.ConditionalCharges where

import Data.Aeson
import Kernel.Prelude
import Kernel.Utils.GenericPretty (PrettyShow, Showable (..))
import Tools.Beam.UtilsTH (mkBeamInstancesForEnumAndList)

data ConditionalChargesCategories
  = SAFETY_PLUS_CHARGES
  | NO_CHARGES
  | NYREGULAR_SUBSCRIPTION_CHARGE
  | BOOKING_DEPOSIT
  | SCHEDULED_RIDE_MIN_WALLET_BALANCE
  deriving (Eq, Ord, ToJSON, FromJSON, ToSchema, Generic, Show, Read)
  deriving (PrettyShow) via Showable ConditionalChargesCategories

$(mkBeamInstancesForEnumAndList ''ConditionalChargesCategories)

isFareComponent :: ConditionalChargesCategories -> Bool
isFareComponent = \case
  SAFETY_PLUS_CHARGES -> True
  NO_CHARGES -> True
  NYREGULAR_SUBSCRIPTION_CHARGE -> True
  BOOKING_DEPOSIT -> False
  SCHEDULED_RIDE_MIN_WALLET_BALANCE -> False
