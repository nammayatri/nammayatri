{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Extra.ConditionalCharges where

import Data.Aeson
import Kernel.Prelude
import Kernel.Utils.GenericPretty (PrettyShow, Showable (..))
import Tools.Beam.UtilsTH (mkBeamInstancesForEnumAndList)

data ConditionalChargesCategories = SAFETY_PLUS_CHARGES | NO_CHARGES | NYREGULAR_SUBSCRIPTION_CHARGE
  deriving (Eq, Ord, ToJSON, FromJSON, ToSchema, Generic, Show, Read)
  deriving (PrettyShow) via Showable ConditionalChargesCategories

$(mkBeamInstancesForEnumAndList ''ConditionalChargesCategories)
