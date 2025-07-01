{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.PickupInstructions where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth

data PickupInstructionsReq = PickupInstructionsReq {pickupInstructions :: [Data.Text.Text]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PickupInstructionsResp = PickupInstructionsResp {pickupInstructions :: [Data.Text.Text]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
