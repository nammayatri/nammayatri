{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Types.UI.PickupInstructions where
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth
import Data.OpenApi (ToSchema)
import qualified Kernel.Prelude



data PickupInstructionResp
    = PickupInstructionResp {audioBase64 :: Kernel.Prelude.Maybe Kernel.Prelude.Text, instruction :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)



