{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.MultimodalConfirm where

import qualified API.Types.UI.FRFSTicketService
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Location
import qualified Domain.Types.LocationAddress
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.JourneyModule.Types
import Servant
import Tools.Auth

data JourneyInfoReq = JourneyInfoReq {legsReq :: [JourneyLegsReq]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyInfoResp = JourneyInfoResp
  { estimatedDistance :: Kernel.Types.Common.Distance,
    estimatedDuration :: Kernel.Types.Common.Seconds,
    estimatedFare :: Kernel.Types.Common.PriceAPIEntity,
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
