{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.CancellationDuesDetails where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Ride
import qualified Domain.Types.RiderDetails
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data CancellationDuesDetails = CancellationDuesDetails
  { cancellationAmount :: Kernel.Types.Common.HighPrecMoney,
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    id :: Kernel.Types.Id.Id Domain.Types.CancellationDuesDetails.CancellationDuesDetails,
    paymentStatus :: Domain.Types.CancellationDuesDetails.CancellationDuesPaymentStatus,
    rideId :: Kernel.Types.Id.Id Domain.Types.Ride.Ride,
    riderId :: Kernel.Types.Id.Id Domain.Types.RiderDetails.RiderDetails,
    updatedAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)
  }
  deriving (Generic)

data CancellationDuesPaymentStatus = PENDING | PAID | WAIVED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''CancellationDuesPaymentStatus))
