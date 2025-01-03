{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.MultimodalConfirm where

import qualified API.Types.UI.FRFSTicketService
import Data.OpenApi (ToSchema)
import qualified Domain.Types.JourneyLeg
import qualified Domain.Types.Location
import qualified Domain.Types.LocationAddress
import qualified Domain.Types.Trip
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.JourneyModule.Types
import Servant
import Tools.Auth

data ExtendLegReq = ExtendLegReq
  { endLeg :: Kernel.Types.Id.Id Domain.Types.JourneyLeg.JourneyLeg,
    endLocation :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    legId :: Kernel.Types.Id.Id Domain.Types.JourneyLeg.JourneyLeg
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyDetails = JourneyDetails {legDetails :: [LegDetails], totalFare :: Kernel.Types.Common.Price}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyInfoReq = JourneyInfoReq {legsReq :: [JourneyLegsReq]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyInfoResp = JourneyInfoResp
  { estimatedDistance :: Kernel.Types.Common.Distance,
    estimatedDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    estimatedFare :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    legs :: [Lib.JourneyModule.Types.LegInfo]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyLegBusInfo = JourneyLegBusInfo
  { busCategory :: Kernel.Prelude.Text,
    destinationStation :: API.Types.UI.FRFSTicketService.FRFSStationAPI,
    frequency :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    originStation :: API.Types.UI.FRFSTicketService.FRFSStationAPI,
    routeNumber :: Kernel.Prelude.Text,
    startTime :: Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyLegInfo
  = Walk JourneyLegWalkInfo
  | Taxi JourneyLegTaxiInfo
  | Bus JourneyLegBusInfo
  | Metro JourneyLegMetroInfo
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyLegMetroInfo = JourneyLegMetroInfo
  { destinationStop :: API.Types.UI.FRFSTicketService.FRFSStationAPI,
    frequency :: Kernel.Types.Common.Seconds,
    lineColor :: Kernel.Prelude.Text,
    lineColorCode :: Kernel.Prelude.Text,
    originStop :: API.Types.UI.FRFSTicketService.FRFSStationAPI
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyLegTaxiInfo = JourneyLegTaxiInfo
  { origin :: Domain.Types.Location.Location,
    providerLogo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    providerName :: Kernel.Prelude.Text,
    stops :: [Domain.Types.Location.Location]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyLegWalkInfo = JourneyLegWalkInfo {destination :: Domain.Types.Location.Location, origin :: Domain.Types.Location.Location}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyLegsReq = JourneyLegsReq {destinationAddress :: Domain.Types.LocationAddress.LocationAddress, legNumber :: Kernel.Prelude.Int, originAddress :: Domain.Types.LocationAddress.LocationAddress}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LegDetails = LegDetails {legFare :: Kernel.Types.Common.Price, legOrder :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LegStatus = LegStatus {legOrder :: Kernel.Prelude.Int, status :: Lib.JourneyModule.Types.JourneyLegStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RiderLocationReq = RiderLocationReq {currTime :: Kernel.Prelude.UTCTime, latLong :: Kernel.External.Maps.Types.LatLong}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SwitchLegReq = SwitchLegReq {currLocation :: Kernel.External.Maps.Types.LatLong, newMode :: Domain.Types.Trip.TravelMode}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
