{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Loyalty where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Servant
import Tools.Auth

data SvpDeductReq = SvpDeductReq {amount :: Kernel.Types.Common.HighPrecMoney, customerId :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
