{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DriverGracePeriodTracker where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PenaltyRule
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id

data DriverGracePeriodTracker = DriverGracePeriodTracker
  { id :: Kernel.Types.Id.Id Domain.Types.DriverGracePeriodTracker.DriverGracePeriodTracker,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    ruleId :: Kernel.Types.Id.Id Domain.Types.PenaltyRule.PenaltyRule,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    offenseCount :: Kernel.Prelude.Int,
    windowStartTime :: Kernel.Prelude.UTCTime,
    windowEndTime :: Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, Eq)
