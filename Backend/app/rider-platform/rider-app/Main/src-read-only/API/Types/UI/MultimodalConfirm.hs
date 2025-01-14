{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.MultimodalConfirm where

import qualified API.Types.UI.FRFSTicketService
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Journey
import qualified Domain.Types.JourneyLeg
import qualified Domain.Types.LocationAddress
import qualified Domain.Types.Trip
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types
import qualified Kernel.External.Payment.Juspay.Types
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

data JourneyBookingPaymentStatus = JourneyBookingPaymentStatus {journeyId :: Kernel.Types.Id.Id Domain.Types.Journey.Journey, paymentOrder :: Kernel.Prelude.Maybe PaymentOrder}
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

data JourneyLegsReq = JourneyLegsReq {destinationAddress :: Domain.Types.LocationAddress.LocationAddress, legNumber :: Kernel.Prelude.Int, originAddress :: Domain.Types.LocationAddress.LocationAddress}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LegStatus = LegStatus {legOrder :: Kernel.Prelude.Int, status :: Lib.JourneyModule.Types.JourneyLegStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PaymentOrder = PaymentOrder {sdkPayload :: Kernel.Prelude.Maybe Kernel.External.Payment.Juspay.Types.CreateOrderResp, status :: API.Types.UI.FRFSTicketService.FRFSBookingPaymentStatusAPI}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RiderLocationReq = RiderLocationReq {currTime :: Kernel.Prelude.UTCTime, latLong :: Kernel.External.Maps.Types.LatLong}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SwitchLegReq = SwitchLegReq {currLocation :: Kernel.External.Maps.Types.LatLong, newMode :: Domain.Types.Trip.TravelMode}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
