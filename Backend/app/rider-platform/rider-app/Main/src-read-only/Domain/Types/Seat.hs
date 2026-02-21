{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Seat where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.SeatLayout
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data Seat = Seat
  { colNo :: Kernel.Prelude.Int,
    id :: Kernel.Types.Id.Id Domain.Types.Seat.Seat,
    isBookable :: Kernel.Prelude.Bool,
    isLadiesOnly :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    rowNo :: Kernel.Prelude.Int,
    seatLabel :: Kernel.Prelude.Text,
    seatLayoutId :: Kernel.Types.Id.Id Domain.Types.SeatLayout.SeatLayout,
    seatType :: Kernel.Prelude.Maybe Domain.Types.Seat.SeatType,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Show, (Generic), (ToJSON), (FromJSON), (Eq), (ToSchema))

data SeatType = WINDOW | AISLE | MIDDLE | SLEEPER_LOWER | SLEEPER_UPPER deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''SeatType))
