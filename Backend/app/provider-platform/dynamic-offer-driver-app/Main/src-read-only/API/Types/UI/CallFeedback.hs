{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Types.UI.CallFeedback where
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth
import Data.OpenApi (ToSchema)
import qualified Kernel.Prelude



data CallFeedbackReq
    = CallFeedbackReq {callId :: Kernel.Prelude.Text, optionIds :: [Kernel.Prelude.Text]}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)



