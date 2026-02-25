{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.EKDLiveCallFeedback where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

newtype EKDLiveCallFeedbackReq = EKDLiveCallFeedbackReq {rideId :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
