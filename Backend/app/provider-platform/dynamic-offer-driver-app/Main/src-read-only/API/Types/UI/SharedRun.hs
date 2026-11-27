{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.SharedRun where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.SharedCabSession
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data SharedRunDetailsRes = SharedRunDetailsRes {acknowledged :: Kernel.Prelude.Bool, session :: Domain.Types.SharedCabSession.SharedCabSession}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
