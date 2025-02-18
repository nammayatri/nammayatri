{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RouteStopFare where

import Data.Aeson
import qualified Domain.Types.FRFSFarePolicy
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data RouteStopFare = RouteStopFare
  { amount :: Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency,
    endStopCode :: Kernel.Prelude.Text,
    farePolicyId :: Kernel.Types.Id.Id Domain.Types.FRFSFarePolicy.FRFSFarePolicy,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    routeCode :: Kernel.Prelude.Text,
    startStopCode :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
