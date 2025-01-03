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

data JourneyLegsReq = JourneyLegsReq {destinationAddress :: Domain.Types.LocationAddress.LocationAddress, legNumber :: Kernel.Prelude.Int, originAddress :: Domain.Types.LocationAddress.LocationAddress}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
