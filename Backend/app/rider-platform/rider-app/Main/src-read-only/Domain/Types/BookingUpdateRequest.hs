{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.BookingUpdateRequest where

import Data.Aeson
import qualified Domain.Types.Booking
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data BookingUpdateRequest = BookingUpdateRequest
  { bookingId :: Kernel.Types.Id.Id Domain.Types.Booking.Booking,
    createdAt :: Kernel.Prelude.UTCTime,
    currentPointLat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    currentPointLon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    errorObj :: Kernel.Prelude.Maybe Domain.Types.BookingUpdateRequest.ErrorObj,
    estimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters,
    estimatedFare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    id :: Kernel.Types.Id.Id Domain.Types.BookingUpdateRequest.BookingUpdateRequest,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    oldEstimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters,
    oldEstimatedFare :: Kernel.Types.Common.HighPrecMoney,
    status :: Domain.Types.BookingUpdateRequest.BookingUpdateRequestStatus,
    totalDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters,
    travelledDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data BookingUpdateRequestStatus = SOFT | CONFIRM deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data ErrorObj = ErrorObj {errorCode :: Kernel.Prelude.Text, errorMessage :: Kernel.Prelude.Text} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''BookingUpdateRequestStatus)

$(mkHttpInstancesForEnum ''BookingUpdateRequestStatus)
