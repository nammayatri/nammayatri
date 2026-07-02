{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.PartnerAuth where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data PartnerSessionReq = PartnerSessionReq {token :: Kernel.Prelude.Text}
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema)

data PartnerSessionRes = PartnerSessionRes
  { isValid :: Kernel.Prelude.Bool,
    mobileNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sessionToken :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema)
