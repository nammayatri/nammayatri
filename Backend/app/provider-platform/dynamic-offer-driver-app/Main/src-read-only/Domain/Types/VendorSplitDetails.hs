{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.VendorSplitDetails where

import Data.Aeson
import qualified Data.Text
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.VehicleVariant
import qualified Domain.Types.VendorFee
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.Types.SpecialLocation
import qualified Tools.Beam.UtilsTH

data VendorSplitDetails = VendorSplitDetails
  { area :: Lib.Types.SpecialLocation.Area,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    splitMethod :: Kernel.Prelude.Maybe Domain.Types.VendorFee.VendorFeeSplitMethod,
    splitType :: Domain.Types.VendorSplitDetails.SplitType,
    splitValue :: Kernel.Prelude.Double,
    vehicleVariant :: Domain.Types.VehicleVariant.VehicleVariant,
    vendorId :: Data.Text.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data SplitType = FIXED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''SplitType)
