{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.SharedCabInternal where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data SharedCabDriverCallNumberResp = SharedCabDriverCallNumberResp {driverNumber :: Kernel.Prelude.Text}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
