{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.FavouriteRoute where

import qualified BecknV2.FRFS.Enums
import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Data.Time
import qualified Data.Time.Calendar
import qualified Data.Time.LocalTime
import qualified Domain.Types.FavouriteRoute
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

data CreateFavouriteRouteReq = CreateFavouriteRouteReq
  { favouriteType :: Domain.Types.FavouriteRoute.FavouriteRouteType,
    fromStopCode :: Kernel.Prelude.Maybe Data.Text.Text,
    notifyOn :: Kernel.Prelude.Maybe [Domain.Types.FavouriteRoute.FavouriteNotifyType],
    notifyingStop :: Kernel.Prelude.Maybe Data.Text.Text,
    recurrenceRuleDays :: Kernel.Prelude.Maybe [Data.Time.Calendar.DayOfWeek],
    routeCode :: Data.Text.Text,
    routeShortName :: Kernel.Prelude.Maybe Data.Text.Text,
    serviceTierType :: Kernel.Prelude.Maybe BecknV2.FRFS.Enums.ServiceTierType,
    shouldNotify :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    toStopCode :: Kernel.Prelude.Maybe Data.Text.Text,
    tripId :: Kernel.Prelude.Maybe Data.Text.Text,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory,
    windowEndTime :: Kernel.Prelude.Maybe Data.Time.LocalTime.TimeOfDay,
    windowStartTime :: Kernel.Prelude.Maybe Data.Time.LocalTime.TimeOfDay
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FavouriteRouteAPIEntity = FavouriteRouteAPIEntity
  { createdAt :: Data.Time.UTCTime,
    favouriteType :: Domain.Types.FavouriteRoute.FavouriteRouteType,
    fromStopCode :: Kernel.Prelude.Maybe Data.Text.Text,
    id :: Kernel.Types.Id.Id Domain.Types.FavouriteRoute.FavouriteRoute,
    notifyOn :: [Domain.Types.FavouriteRoute.FavouriteNotifyType],
    notifyingStop :: Kernel.Prelude.Maybe Data.Text.Text,
    recurrenceRuleDays :: [Data.Time.Calendar.DayOfWeek],
    routeCode :: Data.Text.Text,
    routeShortName :: Kernel.Prelude.Maybe Data.Text.Text,
    serviceTierType :: Kernel.Prelude.Maybe BecknV2.FRFS.Enums.ServiceTierType,
    shouldNotify :: Kernel.Prelude.Bool,
    status :: Domain.Types.FavouriteRoute.FavouriteRouteStatus,
    toStopCode :: Kernel.Prelude.Maybe Data.Text.Text,
    tripId :: Kernel.Prelude.Maybe Data.Text.Text,
    vehicleType :: BecknV2.FRFS.Enums.VehicleCategory,
    windowEndTime :: Kernel.Prelude.Maybe Data.Time.LocalTime.TimeOfDay,
    windowStartTime :: Kernel.Prelude.Maybe Data.Time.LocalTime.TimeOfDay
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateFavouriteRouteReq = UpdateFavouriteRouteReq
  { fromStopCode :: Kernel.Prelude.Maybe Data.Text.Text,
    notifyOn :: [Domain.Types.FavouriteRoute.FavouriteNotifyType],
    notifyingStop :: Kernel.Prelude.Maybe Data.Text.Text,
    recurrenceRuleDays :: [Data.Time.Calendar.DayOfWeek],
    routeShortName :: Kernel.Prelude.Maybe Data.Text.Text,
    serviceTierType :: Kernel.Prelude.Maybe BecknV2.FRFS.Enums.ServiceTierType,
    shouldNotify :: Kernel.Prelude.Bool,
    toStopCode :: Kernel.Prelude.Maybe Data.Text.Text,
    windowEndTime :: Kernel.Prelude.Maybe Data.Time.LocalTime.TimeOfDay,
    windowStartTime :: Kernel.Prelude.Maybe Data.Time.LocalTime.TimeOfDay
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
