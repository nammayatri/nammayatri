{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SavedTrip where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data SavedTrip = SavedTrip
  { id :: Kernel.Types.Id.Id Domain.Types.SavedTrip.SavedTrip,
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    name :: Kernel.Prelude.Text,
    originLat :: Kernel.Prelude.Double,
    originLon :: Kernel.Prelude.Double,
    originAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    destinationLat :: Kernel.Prelude.Double,
    destinationLon :: Kernel.Prelude.Double,
    destinationAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    timeMode :: Domain.Types.SavedTrip.TimeMode,
    targetTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    targetTimeOfDay :: Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay,
    bufferMinutes :: Kernel.Prelude.Int,
    recurrence :: Domain.Types.SavedTrip.TripRecurrence,
    customDays :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    notifyBeforeMinutes :: Kernel.Prelude.Int,
    isActive :: Kernel.Prelude.Bool,
    lastComputedDeparture :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    lastNotifiedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data TimeMode
  = LeaveNow
  | ArriveBy
  | DepartAt
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data TripRecurrence
  = NoRecurrence
  | Daily
  | Weekdays
  | Weekends
  | Custom
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''TimeMode)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''TimeMode)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''TripRecurrence)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''TripRecurrence)
