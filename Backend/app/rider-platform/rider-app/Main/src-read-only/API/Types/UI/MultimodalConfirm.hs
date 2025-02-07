{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.MultimodalConfirm where

import qualified API.Types.UI.FRFSTicketService
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Estimate
import qualified Domain.Types.Journey
import qualified Domain.Types.Location
import qualified Domain.Types.LocationAddress
import Domain.Types.Ride (RideStatus (..))
import qualified Domain.Types.Trip
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Google.MapsClient.Types
import qualified Kernel.External.Maps.Types
import qualified Kernel.External.Payment.Juspay.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.JourneyLeg.Types
import qualified Lib.JourneyModule.Types
import Servant
import Tools.Auth

data ExtendLegGetFareReq = ExtendLegGetFareReq {endLocation :: Kernel.Prelude.Maybe Domain.Types.Location.LocationAPIEntity, startLocation :: Lib.JourneyModule.Types.ExtendLegStartPoint}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ExtendLegGetFareResp = ExtendLegGetFareResp {distance :: Kernel.Types.Common.Distance, duration :: Kernel.Types.Common.Seconds, totalFare :: Kernel.Prelude.Maybe Lib.JourneyModule.Types.GetFareResponse}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ExtendLegReq = ExtendLegReq
  { distance :: Kernel.Types.Common.Distance,
    duration :: Kernel.Types.Common.Seconds,
    endLocation :: Kernel.Prelude.Maybe Domain.Types.Location.LocationAPIEntity,
    fare :: Lib.JourneyModule.Types.GetFareResponse,
    startLocation :: Lib.JourneyModule.Types.ExtendLegStartPoint
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyBookingPaymentStatus = JourneyBookingPaymentStatus {journeyId :: Kernel.Types.Id.Id Domain.Types.Journey.Journey, paymentOrder :: Kernel.Prelude.Maybe PaymentOrder}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyFeedBackForm = JourneyFeedBackForm {additionalFeedBack :: Kernel.Prelude.Maybe Kernel.Prelude.Text, rateTravelMode :: [RateMultiModelTravelModes], rating :: Kernel.Prelude.Maybe Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyInfoResp = JourneyInfoResp
  { estimatedDistance :: Kernel.Types.Common.Distance,
    estimatedDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    estimatedMaxFare :: Kernel.Types.Common.PriceAPIEntity,
    estimatedMinFare :: Kernel.Types.Common.PriceAPIEntity,
    journeyStatus :: Domain.Types.Journey.JourneyStatus,
    legs :: [Lib.JourneyModule.Types.LegInfo],
    unifiedQR :: Kernel.Prelude.Maybe Lib.JourneyModule.Types.UnifiedTicketQR,
    startTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    endTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyStatusResp = JourneyStatusResp {journeyStatus :: Domain.Types.Journey.JourneyStatus, legs :: [LegStatus]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LegStatus = LegStatus
  { legOrder :: Kernel.Prelude.Int,
    status :: Lib.JourneyLeg.Types.JourneyLegStatus,
    userPosition :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    vehiclePosition :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PaymentOrder = PaymentOrder {sdkPayload :: Kernel.Prelude.Maybe Kernel.External.Payment.Juspay.Types.CreateOrderResp, status :: API.Types.UI.FRFSTicketService.FRFSBookingPaymentStatusAPI}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RateMultiModelTravelModes = RateMultiModelTravelModes {isExperienceGood :: Kernel.Prelude.Maybe Kernel.Prelude.Bool, legOrder :: Kernel.Prelude.Int, travelMode :: Domain.Types.Trip.MultimodalTravelMode}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RiderLocationReq = RiderLocationReq {currTime :: Kernel.Prelude.UTCTime, latLong :: Kernel.External.Maps.Types.LatLong}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SwitchLegReq = SwitchLegReq
  { legOrder :: Kernel.Prelude.Int,
    newMode :: Domain.Types.Trip.MultimodalTravelMode,
    originAddress :: Kernel.Prelude.Maybe Domain.Types.LocationAddress.LocationAddress,
    startLocation :: Kernel.Prelude.Maybe Kernel.External.Maps.Google.MapsClient.Types.LatLngV2
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SwitchTaxiReq = SwitchTaxiReq {estimateId :: Kernel.Types.Id.Id Domain.Types.Estimate.Estimate}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
