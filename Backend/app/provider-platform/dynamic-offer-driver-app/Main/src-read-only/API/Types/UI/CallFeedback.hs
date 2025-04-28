{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.CallFeedback where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data CallFeedbackReq = CallFeedbackReq {callId :: Kernel.Prelude.Text, optionIds :: [Kernel.Prelude.Text]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
