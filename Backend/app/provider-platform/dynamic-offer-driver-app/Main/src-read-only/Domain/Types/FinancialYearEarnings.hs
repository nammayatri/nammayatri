{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FinancialYearEarnings where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FinancialYearEarnings = FinancialYearEarnings
  { collectionAmount :: Kernel.Types.Common.HighPrecMoney,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Driver,
    earningsAmount :: Kernel.Types.Common.HighPrecMoney,
    financialYearStart :: Kernel.Prelude.Int,
    gstDeduction :: Kernel.Types.Common.HighPrecMoney,
    id :: Kernel.Types.Id.Id Domain.Types.FinancialYearEarnings.FinancialYearEarnings,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    rideId :: Kernel.Types.Id.Id Domain.Types.Ride.Ride,
    totalEarnings :: Kernel.Types.Common.HighPrecMoney,
    createdAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
