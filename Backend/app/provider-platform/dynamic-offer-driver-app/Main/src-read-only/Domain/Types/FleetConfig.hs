{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FleetConfig where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.DriverCoins.Types
import qualified Tools.Beam.UtilsTH

data FleetConfig = FleetConfig
  { allowAutomaticRoundTripAssignment :: Kernel.Prelude.Bool,
    allowEndingMidRoute :: Kernel.Prelude.Bool,
    allowStartRideFromQR :: Kernel.Prelude.Bool,
    blacklistCoinEvents :: Kernel.Prelude.Maybe [Lib.DriverCoins.Types.DriverCoinsFunctionType],
    directlyStartFirstTripAssignment :: Kernel.Prelude.Bool,
    endRideDistanceThreshold :: Kernel.Types.Common.HighPrecMeters,
    fleetOwnerId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    rideEndApproval :: Kernel.Prelude.Bool,
    unlinkDriverAndVehicleOnTripTermination :: Kernel.Prelude.Bool,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
