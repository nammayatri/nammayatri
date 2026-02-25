{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.MultimodalConfirm where

import qualified API.Types.UI.FRFSTicketService
import qualified BecknV2.FRFS.Enums
import qualified Data.Aeson
import qualified Data.Int
import Data.OpenApi (ToSchema)
import qualified Domain.Types.BookingUpdateRequest
import qualified Domain.Types.Estimate
import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSQuoteCategoryType
import qualified Domain.Types.FRFSTicketBookingStatus
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.Journey
import qualified Domain.Types.JourneyLeg
import qualified Domain.Types.Location
import qualified Domain.Types.LocationAddress
import qualified Domain.Types.MultimodalPreferences
import qualified Domain.Types.RouteDetails
import qualified Domain.Types.RouteStopTimeTable
import qualified Domain.Types.Seat
import qualified Domain.Types.Station
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
import qualified Lib.JourneyModule.State.Types
import qualified Lib.JourneyModule.Types
import qualified Lib.JourneyModule.Utils
import qualified Lib.Payment.Domain.Types.PaymentOrder
import Servant
import qualified SharedLogic.Offer
import qualified Storage.CachedQueries.Merchant.MultiModalBus
import Tools.Auth

data AlternateRouteDetails = AlternateRouteDetails {isLive :: Kernel.Prelude.Bool, routeCode :: Kernel.Prelude.Text, routeShortName :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AvailableRoute = AvailableRoute
  { quoteId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote),
    routeCode :: Kernel.Prelude.Text,
    routeLongName :: Kernel.Prelude.Text,
    routeShortName :: Kernel.Prelude.Text,
    routeTimings :: [Kernel.Types.Common.Seconds],
    serviceTierName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    serviceTierType :: BecknV2.FRFS.Enums.ServiceTierType,
    source :: Domain.Types.RouteStopTimeTable.SourceType
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ChangeStopsReq = ChangeStopsReq
  { journeyId :: Kernel.Types.Id.Id Domain.Types.Journey.Journey,
    legOrder :: Kernel.Prelude.Int,
    newDestinationStation :: Kernel.Prelude.Maybe StationAPIEntity,
    newSourceStation :: Kernel.Prelude.Maybe StationAPIEntity
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ChangeStopsResp = ChangeStopsResp {stationsChanged :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data EffectiveStops = EffectiveStops
  { destStopNameChanged :: Kernel.Prelude.Bool,
    effectiveDestinationStop :: Kernel.Prelude.Text,
    effectiveSourceStop :: Kernel.Prelude.Text,
    requestedDestinationStop :: Kernel.Prelude.Text,
    requestedSourceStop :: Kernel.Prelude.Text,
    sourceStopNameChanged :: Kernel.Prelude.Bool
  }
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

data IntegratedQRReq = IntegratedQRReq {integratedQR :: Lib.JourneyModule.Types.UnifiedTicketQRV2, provider :: Lib.JourneyModule.Types.Provider}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyBookingPaymentStatus = JourneyBookingPaymentStatus
  { gatewayReferenceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    journeyId :: Kernel.Types.Id.Id Domain.Types.Journey.Journey,
    paymentOrder :: Kernel.Prelude.Maybe PaymentOrder
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyConfirmReq = JourneyConfirmReq {enableOffer :: Kernel.Prelude.Maybe Kernel.Prelude.Bool, journeyConfirmReqElements :: [JourneyConfirmReqElement]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyConfirmReqElement = JourneyConfirmReqElement
  { categorySelectionReq :: Kernel.Prelude.Maybe [API.Types.UI.FRFSTicketService.FRFSCategorySelectionReq],
    childTicketQuantity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    crisSdkResponse :: Kernel.Prelude.Maybe API.Types.UI.FRFSTicketService.CrisSdkResponse,
    journeyLegOrder :: Kernel.Prelude.Int,
    seatIds :: Kernel.Prelude.Maybe [Kernel.Types.Id.Id Domain.Types.Seat.Seat],
    skipBooking :: Kernel.Prelude.Bool,
    ticketQuantity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    tripId :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyConfirmResp = JourneyConfirmResp
  { gatewayReferenceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    orderSdkPayload :: Kernel.Prelude.Maybe Kernel.External.Payment.Juspay.Types.CreateOrderResp,
    result :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyFeedBackForm = JourneyFeedBackForm {additionalFeedBack :: Kernel.Prelude.Maybe Kernel.Prelude.Text, rateTravelMode :: [RateMultiModelTravelModes], rating :: Kernel.Prelude.Maybe Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyInfoResp = JourneyInfoResp
  { createdAt :: Kernel.Prelude.UTCTime,
    endTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    estimatedDistance :: Kernel.Types.Common.Distance,
    estimatedDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    estimatedMaxFare :: Kernel.Types.Common.PriceAPIEntity,
    estimatedMinFare :: Kernel.Types.Common.PriceAPIEntity,
    isSingleMode :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    journeyId :: Kernel.Types.Id.Id Domain.Types.Journey.Journey,
    journeyStatus :: Domain.Types.Journey.JourneyStatus,
    legs :: [Lib.JourneyModule.Types.LegInfo],
    merchantOperatingCityName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    offer :: Kernel.Prelude.Maybe SharedLogic.Offer.CumulativeOfferResp,
    paymentOrderShortId :: Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder),
    result :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    startTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    unifiedQR :: Kernel.Prelude.Maybe Lib.JourneyModule.Types.UnifiedTicketQR,
    unifiedQRV2 :: Kernel.Prelude.Maybe Lib.JourneyModule.Types.UnifiedTicketQRV2
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyStatusResp = JourneyStatusResp
  { journeyChangeLogCounter :: Kernel.Prelude.Int,
    journeyPaymentStatus :: Kernel.Prelude.Maybe API.Types.UI.FRFSTicketService.FRFSBookingPaymentStatusAPI,
    journeyStatus :: Domain.Types.Journey.JourneyStatus,
    legs :: [LegStatus]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data KafKaPacket = KafKaPacket
  { client_ip :: Kernel.Prelude.Text,
    dataState :: Kernel.Prelude.Text,
    deviceId :: Kernel.Prelude.Text,
    ign_status :: Kernel.Prelude.Text,
    lat :: Kernel.Prelude.Double,
    long :: Kernel.Prelude.Double,
    provider :: Kernel.Prelude.Text,
    pushedToKafkaAt :: Data.Int.Int64,
    raw :: Kernel.Prelude.Text,
    routeNumber :: Kernel.Prelude.Text,
    serverTime :: Data.Int.Int64,
    signalQuality :: Kernel.Prelude.Text,
    speed :: Kernel.Prelude.Double,
    timestamp :: Data.Int.Int64,
    vehicleNumber :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LegRouteWithLiveVehicle = LegRouteWithLiveVehicle {legOrder :: Kernel.Prelude.Int, routeWithLiveVehicles :: [RouteWithLiveVehicle]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LegServiceTierOptionsResp = LegServiceTierOptionsResp {options :: [Domain.Types.RouteDetails.AvailableRoutesByTier]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LegStatus = LegStatus
  { bookingStatus :: Lib.JourneyModule.State.Types.JourneyBookingStatus,
    fleetNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    legOrder :: Kernel.Prelude.Int,
    mode :: Domain.Types.Trip.MultimodalTravelMode,
    status :: Lib.JourneyLeg.Types.JourneyLegStatus,
    subLegOrder :: Kernel.Prelude.Int,
    trackingStatus :: Lib.JourneyModule.State.Types.TrackingStatus,
    trackingStatusLastUpdatedAt :: Kernel.Prelude.UTCTime,
    userPosition :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    vehiclePositions :: [Lib.JourneyModule.Types.VehiclePosition]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LiveVehicleInfo = LiveVehicleInfo
  { eta :: Kernel.Prelude.Maybe [Storage.CachedQueries.Merchant.MultiModalBus.BusStopETA],
    locationUTCTimestamp :: Kernel.Prelude.UTCTime,
    number :: Kernel.Prelude.Text,
    position :: Kernel.External.Maps.Types.LatLong,
    serviceSubTypes :: Kernel.Prelude.Maybe [BecknV2.FRFS.Enums.ServiceSubType],
    serviceTierName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    serviceTierType :: BecknV2.FRFS.Enums.ServiceTierType
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MultimodalCancelStatusResp = MultimodalCancelStatusResp
  { bookingStatus :: Domain.Types.FRFSTicketBookingStatus.FRFSTicketBookingStatus,
    cancellationCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    isCancellable :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    refundAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MultimodalTicketVerifyReq
  = IntegratedQR IntegratedQRReq
  | SingleQR SingleQRReq
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MultimodalTicketVerifyResp = MultimodalTicketVerifyResp {legInfo :: [Lib.JourneyModule.Types.LegInfo], provider :: Lib.JourneyModule.Types.Provider}
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

data OnboardedVehicleDetailsReq = OnboardedVehicleDetailsReq {vehicleNumber :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PaymentFareUpdate = PaymentFareUpdate
  { category :: Domain.Types.FRFSQuoteCategoryType.FRFSQuoteCategoryType,
    journeyLegOrder :: Kernel.Prelude.Int,
    newFare :: Kernel.Types.Common.PriceAPIEntity,
    oldFare :: Kernel.Types.Common.PriceAPIEntity
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PaymentOrder = PaymentOrder {sdkPayload :: Kernel.Prelude.Maybe Kernel.External.Payment.Juspay.Types.CreateOrderResp, status :: API.Types.UI.FRFSTicketService.FRFSBookingPaymentStatusAPI}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PublicTransportData = PublicTransportData {eligiblePassIds :: Kernel.Prelude.Maybe [Kernel.Prelude.Text], ptcv :: Kernel.Prelude.Text, rs :: [TransportRoute], rsm :: [TransportRouteStopMapping], ss :: [TransportStation]}
  deriving stock (Generic)
  deriving anyclass (ToSchema)

data RateMultiModelTravelModes = RateMultiModelTravelModes
  { isExperienceGood :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    legOrder :: Kernel.Prelude.Int,
    rating :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    travelMode :: Kernel.Prelude.Maybe Domain.Types.Trip.MultimodalTravelMode
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RiderLocationReq = RiderLocationReq {currTime :: Kernel.Prelude.UTCTime, latLong :: Kernel.External.Maps.Types.LatLong}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RouteAvailabilityReq = RouteAvailabilityReq
  { endStopCode :: Kernel.Prelude.Text,
    journeyId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Journey.Journey),
    legOrder :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    onlyLive :: Kernel.Prelude.Bool,
    startStopCode :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RouteAvailabilityResp = RouteAvailabilityResp {availableRoutes :: [AvailableRoute]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RouteCodesWithLeg = RouteCodesWithLeg {legOrder :: Kernel.Prelude.Int, routeCodes :: [Kernel.Prelude.Text]}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RouteServiceabilityReq = RouteServiceabilityReq
  { destinationStopCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    routeCodes :: Kernel.Prelude.Maybe [RouteCodesWithLeg],
    sourceStopCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RouteServiceabilityResp = RouteServiceabilityResp {effectiveStops :: Kernel.Prelude.Maybe EffectiveStops, legs :: [LegRouteWithLiveVehicle]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RouteStopMapping = RouteStopMapping {code :: Kernel.Prelude.Text, lat :: Kernel.Prelude.Double, lon :: Kernel.Prelude.Double, name :: Kernel.Prelude.Text, seqNo :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RouteWithLiveVehicle = RouteWithLiveVehicle {liveVehicles :: [LiveVehicleInfo], routeCode :: Kernel.Prelude.Text, routeShortName :: Kernel.Prelude.Text, schedules :: [ScheduledVehicleInfo]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ScheduledVehicleInfo = ScheduledVehicleInfo
  { availableSeats :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    eta :: Kernel.Prelude.Maybe [Storage.CachedQueries.Merchant.MultiModalBus.BusStopETA],
    locationUTCTimestamp :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    position :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong,
    serviceSubTypes :: Kernel.Prelude.Maybe [BecknV2.FRFS.Enums.ServiceSubType],
    serviceTierName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    serviceTierType :: BecknV2.FRFS.Enums.ServiceTierType,
    tripId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleNumber :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SetRouteNameReq = SetRouteNameReq {journeyId :: Kernel.Types.Id.Id Domain.Types.Journey.Journey, legOrder :: Kernel.Prelude.Int, shortName :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SimilarJourneyLegsResp = SimilarJourneyLegsResp {allLegsLoaded :: Kernel.Prelude.Bool, journeyLegsInfo :: [Lib.JourneyModule.Utils.JourneyLegOption]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SingleQRReq = SingleQRReq {provider :: Lib.JourneyModule.Types.Provider, tickets :: [Kernel.Prelude.Text]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data StationAPIEntity = StationAPIEntity {stopCode :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SwitchFRFSTierReq = SwitchFRFSTierReq {quoteId :: Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SwitchJourneyLegReq = SwitchJourneyLegReq {journeyLegId :: Kernel.Types.Id.Id Domain.Types.JourneyLeg.JourneyLeg}
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

data SwitchRouteReq = SwitchRouteReq
  { journeyId :: Kernel.Types.Id.Id Domain.Types.Journey.Journey,
    legOrder :: Kernel.Prelude.Int,
    quoteId :: Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote,
    routeCode :: Kernel.Prelude.Text,
    routeLongName :: Kernel.Prelude.Text,
    routeShortName :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SwitchTaxiReq = SwitchTaxiReq {estimateId :: Kernel.Types.Id.Id Domain.Types.Estimate.Estimate}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TowerInfo = TowerInfo
  { areaCode :: Kernel.Prelude.Int,
    cellId :: Kernel.Prelude.Text,
    cellType :: Kernel.Prelude.Text,
    isRegistered :: Kernel.Prelude.Bool,
    networkType :: Kernel.Prelude.Text,
    signalStrength :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TowerInfoReq = TowerInfoReq {latLngAccuracy :: Kernel.Prelude.Double, timeStamp :: Kernel.Prelude.UTCTime, towerInfo :: [TowerInfo], userLat :: Kernel.Prelude.Double, userLng :: Kernel.Prelude.Double}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TransportRoute = TransportRoute
  { cd :: Kernel.Prelude.Text,
    clr :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    dTC :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    ibc :: Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig,
    lN :: Kernel.Prelude.Text,
    sN :: Kernel.Prelude.Text,
    sst :: Kernel.Prelude.Maybe [BecknV2.FRFS.Enums.ServiceSubType],
    st :: Kernel.Prelude.Maybe BecknV2.FRFS.Enums.ServiceTierType,
    stC :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    stn :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vt :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToSchema)

data TransportRouteStopMapping = TransportRouteStopMapping {ibc :: Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig, rc :: Kernel.Prelude.Text, sc :: Kernel.Prelude.Text, sn :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToSchema)

data TransportStation = TransportStation
  { ad :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    cd :: Kernel.Prelude.Text,
    gi :: Kernel.Prelude.Maybe [Domain.Types.Station.Gate],
    gj :: Kernel.Prelude.Maybe Data.Aeson.Value,
    hin :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    ibc :: Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig,
    ln :: Kernel.Prelude.Double,
    lt :: Kernel.Prelude.Double,
    nm :: Kernel.Prelude.Text,
    rgn :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sgstdDest :: Kernel.Prelude.Maybe [Domain.Types.StationType.SuggestedStations],
    vt :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToSchema)

data UpdateBusLocationReq = UpdateBusLocationReq {lat :: Kernel.Prelude.Double, long :: Kernel.Prelude.Double, timestamp :: Kernel.Prelude.Double}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdatePaymentOrderReq = UpdatePaymentOrderReq {childTicketQuantity :: Kernel.Prelude.Int, quantity :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdatePaymentOrderResp = UpdatePaymentOrderResp {sdkPayload :: Kernel.Prelude.Maybe Kernel.External.Payment.Juspay.Types.SDKPayloadDetails}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
