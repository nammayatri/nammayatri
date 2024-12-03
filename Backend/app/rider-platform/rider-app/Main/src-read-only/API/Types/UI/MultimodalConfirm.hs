{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.MultimodalConfirm where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Trip
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data JourneyConfirmReq = JourneyConfirmReq {journeyConfirmReqElements :: [JourneyConfirmReqElement]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyConfirmReqElement = JourneyConfirmReqElement {journeyLegOrder :: Kernel.Prelude.Int, skipBooking :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyInfoElement = JourneyInfoElement {journeyLegOrder :: Kernel.Prelude.Int, mode :: Domain.Types.Trip.TravelMode, pricingId :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyInfoResp = JourneyInfoResp {journeyInfoElements :: [JourneyInfoElement]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
