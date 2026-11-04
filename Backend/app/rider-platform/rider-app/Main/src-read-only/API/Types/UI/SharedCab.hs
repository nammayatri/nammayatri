{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.SharedCab where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth

data SharedCabSkipReason
  = FULL
  | OTHER
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SharedCabSkipReq = SharedCabSkipReq {reason :: SharedCabSkipReason}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
