{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.CorporatePolicy (module Domain.Types.CorporatePolicy, module ReExport) where

import Data.Aeson
import Domain.Types.Extra.CorporatePolicy as ReExport
import qualified Domain.Types.CorporateEntity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data CorporatePolicy = CorporatePolicy
  { id :: Kernel.Types.Id.Id Domain.Types.CorporatePolicy.CorporatePolicy,
    corporateEntityId :: Kernel.Types.Id.Id Domain.Types.CorporateEntity.CorporateEntity,
    name :: Kernel.Prelude.Text,
    policyType :: Domain.Types.CorporatePolicy.CorporatePolicyType,
    maxFarePerTrip :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    maxMonthlyBudgetPerEmployee :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    allowedServiceTiers :: Kernel.Prelude.Text,
    requiresApproval :: Kernel.Prelude.Bool,
    nightShiftSafetyEnabled :: Kernel.Prelude.Bool,
    womenSafetyRulesEnabled :: Kernel.Prelude.Bool,
    surgeCap :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    isActive :: Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data CorporatePolicyType = RIDE_BUDGET | VEHICLE_TIER | APPROVAL | SAFETY
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''CorporatePolicyType)
