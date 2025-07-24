{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Fleet.Endpoints.LiveMap where

import qualified Dashboard.Common
import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Domain.Types.VehicleVariant
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data MapDriverInfoRes = MapDriverInfoRes
  { driverName :: Kernel.Prelude.Text,
    driverStatus :: Kernel.Prelude.Maybe Status,
    vehicleNumber :: Kernel.Prelude.Text,
    vehicleVariant :: Domain.Types.VehicleVariant.VehicleVariant,
    position :: Kernel.External.Maps.Types.LatLong,
    mobileCountryCode :: Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Text,
    rideRelatedInfo :: RideRelatedInfo
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideRelatedInfo = RideRelatedInfo
  { tripStartTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    fare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    source :: Kernel.Prelude.Text,
    destination :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Status
  = ON_PICKUP
  | ON_RIDE
  | ACTIVE
  | INACTIVE
  | ONLINE
  | SILENT
  | OFFLINE
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("liveMap" :> GetLiveMapDriversHelper)

type GetLiveMapDrivers =
  ( "drivers" :> Capture "radius" Kernel.Types.Common.Meters :> QueryParam "fleetOwnerId" Kernel.Prelude.Text
      :> QueryParam
           "driverIdForRadius"
           (Kernel.Types.Id.Id Dashboard.Common.Driver)
      :> QueryParam "point" Kernel.External.Maps.Types.LatLong
      :> Get '[JSON] [MapDriverInfoRes]
  )

type GetLiveMapDriversHelper =
  ( "drivers" :> Capture "radius" Kernel.Types.Common.Meters :> Capture "requestorId" Kernel.Prelude.Text
      :> QueryParam
           "fleetOwnerId"
           Kernel.Prelude.Text
      :> QueryParam "driverIdForRadius" (Kernel.Types.Id.Id Dashboard.Common.Driver)
      :> QueryParam
           "point"
           Kernel.External.Maps.Types.LatLong
      :> Get
           '[JSON]
           [MapDriverInfoRes]
  )

newtype LiveMapAPIs = LiveMapAPIs {getLiveMapDrivers :: Kernel.Types.Common.Meters -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong -> EulerHS.Types.EulerClient [MapDriverInfoRes]}

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
