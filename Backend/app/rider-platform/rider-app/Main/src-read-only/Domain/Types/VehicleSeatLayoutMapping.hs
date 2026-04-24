{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.VehicleSeatLayoutMapping where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.SeatLayout
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data VehicleSeatLayoutMapping = VehicleSeatLayoutMapping
  { gtfsId :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.VehicleSeatLayoutMapping.VehicleSeatLayoutMapping,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    seatLayoutId :: Kernel.Types.Id.Id Domain.Types.SeatLayout.SeatLayout,
    seatSelectionType :: Kernel.Prelude.Maybe Domain.Types.VehicleSeatLayoutMapping.SeatSelectionType,
    vehicleNo :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, Eq, ToSchema)

data SeatSelectionType = UNRESERVED | AUTO_ASSIGNED | USER_SELECTED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''SeatSelectionType)

$(mkHttpInstancesForEnum ''SeatSelectionType)
