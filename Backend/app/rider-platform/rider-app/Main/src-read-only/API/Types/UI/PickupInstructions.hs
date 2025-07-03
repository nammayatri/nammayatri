{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.PickupInstructions where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data ClosestPickupInstructionResp = ClosestPickupInstructionResp {instruction :: Kernel.Prelude.Maybe Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PickupInstructionsReq = PickupInstructionsReq {instruction :: Data.Text.Text, lat :: Kernel.Prelude.Double, lon :: Kernel.Prelude.Double}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
