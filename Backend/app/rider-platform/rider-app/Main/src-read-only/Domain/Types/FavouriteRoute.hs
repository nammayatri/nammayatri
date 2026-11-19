{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FavouriteRoute (module Domain.Types.FavouriteRoute, module ReExport) where

import qualified BecknV2.FRFS.Enums
import Data.Aeson
import qualified Data.Time.Calendar
import qualified Data.Time.LocalTime
import Domain.Types.Extra.FavouriteRoute as ReExport
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data FavouriteRoute = FavouriteRoute
  { createdAt :: Kernel.Prelude.UTCTime,
    favouriteType :: Domain.Types.FavouriteRoute.FavouriteRouteType,
    fromStopCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.FavouriteRoute.FavouriteRoute,
    integratedBppConfigId :: Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    notifyOn :: [Domain.Types.FavouriteRoute.FavouriteNotifyType],
    notifyingStop :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    recurrenceRuleDays :: [Data.Time.Calendar.DayOfWeek],
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    routeCode :: Kernel.Prelude.Text,
    routeShortName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    serviceTierType :: Kernel.Prelude.Maybe BecknV2.FRFS.Enums.ServiceTierType,
    shouldNotify :: Kernel.Prelude.Bool,
    status :: Domain.Types.FavouriteRoute.FavouriteRouteStatus,
    toStopCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    tripId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory,
    windowEndTime :: Kernel.Prelude.Maybe Data.Time.LocalTime.TimeOfDay,
    windowStartTime :: Kernel.Prelude.Maybe Data.Time.LocalTime.TimeOfDay
  }
  deriving (Generic, (Eq), (Show), (ToJSON), (FromJSON), (ToSchema))

data FavouriteNotifyType = TRIP_STARTED | APPROACHING deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data FavouriteRouteStatus = ACTIVE | INACTIVE deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data FavouriteRouteType = TRIP | PATTERN deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data NotifyTripStartedSource = TRIP_ACTION | TRACKING deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''FavouriteNotifyType))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''FavouriteRouteStatus))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''FavouriteRouteType))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''NotifyTripStartedSource))

$(mkHttpInstancesForEnum (''NotifyTripStartedSource))
