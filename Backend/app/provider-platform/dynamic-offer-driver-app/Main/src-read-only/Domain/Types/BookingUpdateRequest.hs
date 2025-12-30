{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.BookingUpdateRequest where

import Data.Aeson
import qualified Domain.Types.Booking
import qualified Domain.Types.FareParameters
import qualified Domain.Types.FarePolicy
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data BookingUpdateRequest = BookingUpdateRequest
  { bapBookingUpdateRequestId :: Kernel.Prelude.Text,
    bookingId :: Kernel.Types.Id.Id Domain.Types.Booking.Booking,
    createdAt :: Kernel.Prelude.UTCTime,
    currentPointLat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    currentPointLon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    estimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters,
    estimatedFare :: Kernel.Types.Common.HighPrecMoney,
    fareParamsId :: Kernel.Types.Id.Id Domain.Types.FareParameters.FareParameters,
    farePolicyId :: Kernel.Types.Id.Id Domain.Types.FarePolicy.FarePolicy,
    getRouteReq :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.BookingUpdateRequest.BookingUpdateRequest,
    maxEstimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    oldEstimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters,
    oldEstimatedFare :: Kernel.Types.Common.HighPrecMoney,
    oldFareParamsId :: Kernel.Types.Id.Id Domain.Types.FareParameters.FareParameters,
    oldMaxEstimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters,
    routeInfoResp :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    snapToRoadFailed :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    status :: Domain.Types.BookingUpdateRequest.BookingUpdateRequestStatus,
    totalDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters,
    travelledDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters,
    updatedAt :: Kernel.Prelude.UTCTime,
    validTill :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show)

data BookingUpdateRequestStatus = SOFT | DRIVER_ACCEPTED | DRIVER_REJECTED | USER_CONFIRMED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''BookingUpdateRequestStatus))

$(mkHttpInstancesForEnum (''BookingUpdateRequestStatus))
