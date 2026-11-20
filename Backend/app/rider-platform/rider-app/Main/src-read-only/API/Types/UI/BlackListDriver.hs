{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.BlackListDriver where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data BlackListDriverReq = BlackListDriverReq {blackListed :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BlackListedDriverResp = BlackListedDriverResp {driverName :: Data.Text.Text, driverPhone :: Kernel.Prelude.Maybe Data.Text.Text, driverRating :: Kernel.Prelude.Maybe Kernel.Prelude.Double, id :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
