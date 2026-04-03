{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Types.UI.CancellationReasonLookup where
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth
import Data.OpenApi (ToSchema)
import qualified Kernel.Prelude



data CancellationReasonResp
    = CancellationReasonResp {reasonCode :: Kernel.Prelude.Text, reasonMessage :: Kernel.Prelude.Text}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)



