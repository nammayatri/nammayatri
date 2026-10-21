{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.BillingModelSwitchHistory where

import Data.Aeson
import qualified Domain.Types.Extra.Plan
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data BillingModelSwitchHistory = BillingModelSwitchHistory
  { acknowledgedUnusedCredits :: Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    fromModel :: Domain.Types.Extra.Plan.ServiceNames,
    id :: Kernel.Types.Id.Id Domain.Types.BillingModelSwitchHistory.BillingModelSwitchHistory,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    requestedBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    source :: Domain.Types.BillingModelSwitchHistory.BillingModelSwitchSource,
    toModel :: Domain.Types.Extra.Plan.ServiceNames,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data BillingModelSwitchSource = DRIVER | DASHBOARD deriving (Show, (Eq), (Ord), (Read), (Generic), (ToJSON), (FromJSON), (ToSchema))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''BillingModelSwitchSource))
