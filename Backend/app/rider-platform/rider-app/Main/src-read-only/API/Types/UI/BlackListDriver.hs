{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.BlackListDriver where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data BlackListDriverReq = BlackListDriverReq {blackListed :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
