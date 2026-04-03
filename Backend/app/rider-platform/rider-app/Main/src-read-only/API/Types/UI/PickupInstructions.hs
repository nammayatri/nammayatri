{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Types.UI.PickupInstructions where
import Kernel.Utils.TH
import Data.Aeson
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth
import Data.OpenApi (ToSchema)
import qualified Kernel.Prelude
import qualified Data.Text
import qualified EulerHS.Prelude



data ClosestPickupInstructionResp
    = ClosestPickupInstructionResp {audioBase64 :: Kernel.Prelude.Maybe Data.Text.Text, instruction :: Kernel.Prelude.Maybe Data.Text.Text}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
data DeleteTarget
    = Instruction | Audio
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)
data PickupInstructionsReq
    = PickupInstructionsReq {file :: Kernel.Prelude.Maybe EulerHS.Prelude.FilePath, instruction :: Data.Text.Text, lat :: Kernel.Prelude.Double, lon :: Kernel.Prelude.Double}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum (''DeleteTarget))

