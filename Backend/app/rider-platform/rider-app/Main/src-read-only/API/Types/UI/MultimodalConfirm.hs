{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.MultimodalConfirm where

import qualified API.Types.UI.FRFSTicketService
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Estimate
import qualified Domain.Types.Journey
import qualified Domain.Types.JourneyLeg
import qualified Domain.Types.Trip
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Types
import qualified Kernel.External.Payment.Juspay.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.JourneyLeg.Types
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

data JourneyInfoResp = JourneyInfoResp
  { estimatedDistance :: Kernel.Types.Common.Distance,
    estimatedDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    estimatedMaxFare :: Kernel.Types.Common.PriceAPIEntity,
    estimatedMinFare :: Kernel.Types.Common.PriceAPIEntity,
    legs :: [Lib.JourneyModule.Types.LegInfo]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype JourneyStatus = JourneyStatus {legs :: [LegStatus]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LegStatus = LegStatus {legOrder :: Kernel.Prelude.Int, status :: Lib.JourneyLeg.Types.JourneyLegStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PaymentOrder = PaymentOrder {sdkPayload :: Kernel.Prelude.Maybe Kernel.External.Payment.Juspay.Types.CreateOrderResp, status :: API.Types.UI.FRFSTicketService.FRFSBookingPaymentStatusAPI}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RiderLocationReq = RiderLocationReq {currTime :: Kernel.Prelude.UTCTime, latLong :: Kernel.External.Maps.Types.LatLong}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SwitchLegReq = SwitchLegReq {currLocation :: Kernel.External.Maps.Types.LatLong, newMode :: Domain.Types.Trip.MultimodalTravelMode}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SwitchTaxiReq = SwitchTaxiReq {estimateId :: Kernel.Types.Id.Id Domain.Types.Estimate.Estimate}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
