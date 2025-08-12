{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.PickupInstructions where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data PickupInstructionResp = PickupInstructionResp {audioBase64 :: Kernel.Prelude.Maybe Kernel.Prelude.Text, instruction :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
