{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.PickupInstructions where

import Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Text
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Prelude
import qualified Kernel.Prelude
import Kernel.Utils.TH
import Servant
import Tools.Auth

data ClosestPickupInstructionResp = ClosestPickupInstructionResp {audioBase64 :: Kernel.Prelude.Maybe Data.Text.Text, instruction :: Kernel.Prelude.Maybe Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DeleteTarget
  = Instruction
  | Audio
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data PickupInstructionsReq = PickupInstructionsReq {file :: Kernel.Prelude.Maybe EulerHS.Prelude.FilePath, instruction :: Data.Text.Text, lat :: Kernel.Prelude.Double, lon :: Kernel.Prelude.Double}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''DeleteTarget)
