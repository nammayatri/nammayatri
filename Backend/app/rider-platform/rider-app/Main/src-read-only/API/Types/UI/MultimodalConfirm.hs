{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.MultimodalConfirm where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data JourneyConfirmReq = JourneyConfirmReq {journeyConfirmReqElements :: [API.Types.UI.MultimodalConfirm.JourneyConfirmReqElement]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data JourneyConfirmReqElement = JourneyConfirmReqElement {journeyLegOrder :: Kernel.Prelude.Int, skipBooking :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
