{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Fleet.Endpoints.LiveMap where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Common
import Servant
import Servant.Client

data FleetMapDriverInfoRes = FleetMapDriverInfoRes
  { driverName :: Kernel.Prelude.Text,
    driverStatus :: Status,
    todaySummary :: TodaySummary,
    position :: Kernel.External.Maps.Types.LatLong,
    source :: Kernel.Prelude.Text,
    destination :: Kernel.Prelude.Text,
    mobileCountryCode :: Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MapDriverInfoRes
  = FleetMapDriverInfo FleetMapDriverInfoRes
  | OperatorMapDriverInfo OperatorMapDriverInfoRes
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OperatorMapDriverInfoRes = OperatorMapDriverInfoRes
  { driverName :: Kernel.Prelude.Text,
    driverStatus :: Status,
    vehicleNumber :: Kernel.Prelude.Text,
    vehicleStatus :: Status,
    position :: Kernel.External.Maps.Types.LatLong,
    source :: Kernel.Prelude.Text,
    destination :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Status
  = TO_PICKUP
  | ON_TRIP
  | ONLINE
  | SILENT
  | OFFLINE
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TodaySummary = TodaySummary
  { tripStatus :: Status,
    tripsCompletedCount :: Kernel.Prelude.Int,
    earnings :: Kernel.Types.Common.HighPrecMoney,
    totalDistance :: Kernel.Types.Common.Meters,
    tripBalanceLeft :: Kernel.Prelude.Int,
    tripsCancelled :: Kernel.Prelude.Int,
    tripsPassed :: Kernel.Prelude.Int,
    tripsScheduled :: Kernel.Prelude.Int,
    onlineDuration :: Kernel.Types.Common.Seconds
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("liveMap" :> GetLiveMapDriversHelper)

type GetLiveMapDrivers = ("drivers" :> QueryParam "fleetOwnerId" Kernel.Prelude.Text :> Get '[JSON] [MapDriverInfoRes])

type GetLiveMapDriversHelper = ("drivers" :> Capture "requestorId" Kernel.Prelude.Text :> QueryParam "fleetOwnerId" Kernel.Prelude.Text :> Get '[JSON] [MapDriverInfoRes])

newtype LiveMapAPIs = LiveMapAPIs {getLiveMapDrivers :: Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient [MapDriverInfoRes]}

mkLiveMapAPIs :: (Client EulerHS.Types.EulerClient API -> LiveMapAPIs)
mkLiveMapAPIs liveMapClient = (LiveMapAPIs {..})
  where
    getLiveMapDrivers = liveMapClient

data LiveMapUserActionType
  = GET_LIVE_MAP_DRIVERS
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON LiveMapUserActionType where
  toJSON GET_LIVE_MAP_DRIVERS = Data.Aeson.String "GET_LIVE_MAP_DRIVERS"

instance FromJSON LiveMapUserActionType where
  parseJSON (Data.Aeson.String "GET_LIVE_MAP_DRIVERS") = pure GET_LIVE_MAP_DRIVERS
  parseJSON _ = fail "GET_LIVE_MAP_DRIVERS expected"

$(Data.Singletons.TH.genSingletons [''LiveMapUserActionType])
