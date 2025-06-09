{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.MultimodalConfirm where

import qualified API.Types.UI.FRFSTicketService
import qualified BecknV2.FRFS.Enums
import Data.OpenApi (ToSchema)
import qualified Domain.Types.BookingUpdateRequest
import qualified Domain.Types.Estimate
import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.Journey
import qualified Domain.Types.Location
import qualified Domain.Types.LocationAddress
import qualified Domain.Types.MultimodalPreferences
import qualified Domain.Types.StationType
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
import qualified Lib.JourneyModule.Utils
import Servant
import Tools.Auth

data CrisSdkResponse = CrisSdkResponse {bookAuthCode :: Kernel.Prelude.Text, osBuildVersion :: Kernel.Prelude.Text, osType :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ExtendLegGetFareReq = ExtendLegGetFareReq {endLocation :: Kernel.Prelude.Maybe Domain.Types.Location.LocationAPIEntity, startLocation :: Lib.JourneyModule.Types.ExtendLegStartPoint}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ExtendLegGetFareResp = ExtendLegGetFareResp
  { bookingUpdateRequestId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.BookingUpdateRequest.BookingUpdateRequest),
    distance :: Kernel.Types.Common.Distance,
    duration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    totalFare :: Kernel.Prelude.Maybe Lib.JourneyModule.Types.GetFareResponse
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ExtendLegReq = ExtendLegReq
  { bookingUpdateRequestId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.BookingUpdateRequest.BookingUpdateRequest),
    distance :: Kernel.Types.Common.Distance,
    duration :: Kernel.Types.Common.Seconds,
    endLocation :: Kernel.Prelude.Maybe Domain.Types.Location.LocationAPIEntity,
    fare :: Lib.JourneyModule.Types.GetFareResponse,
    startLocation :: Lib.JourneyModule.Types.ExtendLegStartPoint
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyBookingPaymentStatus = JourneyBookingPaymentStatus
  { journeyId :: Kernel.Types.Id.Id Domain.Types.Journey.Journey,
    paymentFareUpdate :: Kernel.Prelude.Maybe [PaymentFareUpdate],
    paymentOrder :: Kernel.Prelude.Maybe PaymentOrder
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyConfirmReq = JourneyConfirmReq {journeyConfirmReqElements :: [JourneyConfirmReqElement]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyConfirmReqElement = JourneyConfirmReqElement
  { childTicketQuantity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    crisSdkResponse :: Kernel.Prelude.Maybe CrisSdkResponse,
    journeyLegOrder :: Kernel.Prelude.Int,
    skipBooking :: Kernel.Prelude.Bool,
    ticketQuantity :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyFeedBackForm = JourneyFeedBackForm {additionalFeedBack :: Kernel.Prelude.Maybe Kernel.Prelude.Text, rateTravelMode :: [RateMultiModelTravelModes], rating :: Kernel.Prelude.Maybe Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyInfoResp = JourneyInfoResp
  { crisSdkToken :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    endTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    estimatedDistance :: Kernel.Types.Common.Distance,
    estimatedDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    estimatedMaxFare :: Kernel.Types.Common.PriceAPIEntity,
    estimatedMinFare :: Kernel.Types.Common.PriceAPIEntity,
    journeyId :: Kernel.Types.Id.Id Domain.Types.Journey.Journey,
    journeyStatus :: Domain.Types.Journey.JourneyStatus,
    legs :: [Lib.JourneyModule.Types.LegInfo],
    merchantOperatingCityName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    startTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    unifiedQR :: Kernel.Prelude.Maybe Lib.JourneyModule.Types.UnifiedTicketQR
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyInfoRespWithFare = JourneyInfoRespWithFare {journeyInfoResponse :: JourneyInfoResp, totalFare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyStatusResp = JourneyStatusResp
  { journeyChangeLogCounter :: Kernel.Prelude.Int,
    journeyPaymentStatus :: Kernel.Prelude.Maybe API.Types.UI.FRFSTicketService.FRFSBookingPaymentStatusAPI,
    journeyStatus :: Domain.Types.Journey.JourneyStatus,
    legs :: [LegStatus]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LegServiceTierOptionsResp = LegServiceTierOptionsResp {options :: [Lib.JourneyModule.Utils.AvailableRoutesByTier]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LegStatus = LegStatus
  { legOrder :: Kernel.Prelude.Int,
    mode :: Domain.Types.Trip.MultimodalTravelMode,
    status :: Lib.JourneyLeg.Types.JourneyLegStatus,
    subLegOrder :: Kernel.Prelude.Int,
    userPosition :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    vehiclePositions :: [Lib.JourneyModule.Types.VehiclePosition]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MultimodalTransitOptionData = MultimodalTransitOptionData {duration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds, travelModes :: [Domain.Types.Trip.MultimodalTravelMode]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MultimodalTransitOptionsReq = MultimodalTransitOptionsReq {destLatLong :: Kernel.External.Maps.Types.LatLong, sourceLatLong :: Kernel.External.Maps.Types.LatLong}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MultimodalTransitOptionsResp = MultimodalTransitOptionsResp {options :: [MultimodalTransitOptionData]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MultimodalUserPreferences = MultimodalUserPreferences
  { allowedTransitModes :: [Domain.Types.Trip.MultimodalTravelMode],
    busTransitTypes :: Kernel.Prelude.Maybe [BecknV2.FRFS.Enums.ServiceTierType],
    journeyOptionsSortingType :: Kernel.Prelude.Maybe Domain.Types.MultimodalPreferences.JourneyOptionsSortingType,
    subwayTransitTypes :: Kernel.Prelude.Maybe [BecknV2.FRFS.Enums.ServiceTierType]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PaymentFareUpdate = PaymentFareUpdate {journeyLegOrder :: Kernel.Prelude.Int, newFare :: Kernel.Types.Common.PriceAPIEntity, oldFare :: Kernel.Types.Common.PriceAPIEntity}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PaymentOrder = PaymentOrder {sdkPayload :: Kernel.Prelude.Maybe Kernel.External.Payment.Juspay.Types.CreateOrderResp, status :: API.Types.UI.FRFSTicketService.FRFSBookingPaymentStatusAPI}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PublicTransportData = PublicTransportData {ptcv :: Kernel.Prelude.Text, rs :: [TransportRoute], rsm :: [TransportRouteStopMapping], ss :: [TransportStation]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RateMultiModelTravelModes = RateMultiModelTravelModes {isExperienceGood :: Kernel.Prelude.Maybe Kernel.Prelude.Bool, legOrder :: Kernel.Prelude.Int, travelMode :: Kernel.Prelude.Maybe Domain.Types.Trip.MultimodalTravelMode}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RiderLocationReq = RiderLocationReq {currTime :: Kernel.Prelude.UTCTime, latLong :: Kernel.External.Maps.Types.LatLong}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SwitchFRFSTierReq = SwitchFRFSTierReq {quoteId :: Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote}
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

data TransportRoute = TransportRoute
  { cd :: Kernel.Prelude.Text,
    clr :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    dTC :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    lN :: Kernel.Prelude.Text,
    sN :: Kernel.Prelude.Text,
    stC :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    vt :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TransportRouteStopMapping = TransportRouteStopMapping {rc :: Kernel.Prelude.Text, sc :: Kernel.Prelude.Text, sn :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TransportStation = TransportStation
  { ad :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    cd :: Kernel.Prelude.Text,
    hin :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    ln :: Kernel.Prelude.Double,
    lt :: Kernel.Prelude.Double,
    nm :: Kernel.Prelude.Text,
    rgn :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sgstdDest :: Kernel.Prelude.Maybe [Domain.Types.StationType.SuggestedStations],
    vt :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdatePaymentOrderReq = UpdatePaymentOrderReq {childTicketQuantity :: Kernel.Prelude.Int, quantity :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdatePaymentOrderResp = UpdatePaymentOrderResp {sdkPayload :: Kernel.Prelude.Maybe Kernel.External.Payment.Juspay.Types.SDKPayloadDetails}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
