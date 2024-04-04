{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Ride where

import Data.Aeson
import qualified Domain.Types.Booking
import qualified Domain.Types.Client
import qualified Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest
import qualified Domain.Types.FareParameters
import qualified Domain.Types.Location
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified IssueManagement.Domain.Types.MediaFile
import qualified Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data Ride = Ride
  { bookingId :: Kernel.Types.Id.Id Domain.Types.Booking.Booking,
    chargeableDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    clientId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Client.Client),
    createdAt :: Kernel.Prelude.UTCTime,
    distanceCalculationFailed :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    driverArrivalTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    driverDeviatedFromRoute :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    driverGoHomeRequestId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest.DriverGoHomeRequest),
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    enableFrequentLocationUpdates :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    endOdometerReading :: Kernel.Prelude.Maybe Domain.Types.Ride.OdometerReading,
    endOtp :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fare :: Kernel.Prelude.Maybe Kernel.Types.Common.Money,
    fareParametersId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.FareParameters.FareParameters),
    fromLocation :: Domain.Types.Location.Location,
    id :: Kernel.Types.Id.Id Domain.Types.Ride.Ride,
    isFreeRide :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity,
    numberOfDeviation :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    numberOfOsrmSnapToRoadCalls :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    numberOfSnapToRoadCalls :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    otp :: Kernel.Prelude.Text,
    pickupDropOutsideOfThreshold :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    safetyAlertTriggered :: Kernel.Prelude.Bool,
    shortId :: Kernel.Types.Id.ShortId Domain.Types.Ride.Ride,
    startOdometerReading :: Kernel.Prelude.Maybe Domain.Types.Ride.OdometerReading,
    status :: Domain.Types.Ride.RideStatus,
    toLocation :: Kernel.Prelude.Maybe Domain.Types.Location.Location,
    tollCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    trackingUrl :: Kernel.Types.Common.BaseUrl,
    traveledDistance :: Kernel.Types.Common.HighPrecMeters,
    tripEndPos :: Kernel.Prelude.Maybe Kernel.External.Maps.LatLong,
    tripEndTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    tripStartPos :: Kernel.Prelude.Maybe Kernel.External.Maps.LatLong,
    tripStartTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    uiDistanceCalculationWithAccuracy :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    uiDistanceCalculationWithoutAccuracy :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

data OdometerReading = OdometerReading {fileId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile), value :: Kernel.Types.Common.Centesimal}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq)

data RideStatus = NEW | INPROGRESS | COMPLETED | CANCELLED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''RideStatus)

$(mkHttpInstancesForEnum ''RideStatus)
