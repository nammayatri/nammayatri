{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
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
  { id :: Kernel.Types.Id.Id Domain.Types.BookingUpdateRequest.BookingUpdateRequest,
    bookingId :: Kernel.Types.Id.Id Domain.Types.Booking.Booking,
    status :: Domain.Types.BookingUpdateRequest.BookingUpdateRequestStatus,
    travelledDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters,
    estimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters,
    estimatedFare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    totalDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters,
    currentPointLat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    currentPointLon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    oldEstimatedFare :: Kernel.Types.Common.HighPrecMoney,
    oldEstimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters,
    errorObj :: Kernel.Prelude.Maybe Domain.Types.BookingUpdateRequest.ErrorObj,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data BookingUpdateRequestStatus = SOFT | CONFIRM deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data ErrorObj = ErrorObj {errorCode :: Kernel.Prelude.Text, errorMessage :: Kernel.Prelude.Text} deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''BookingUpdateRequestStatus)

$(mkHttpInstancesForEnum ''BookingUpdateRequestStatus)
