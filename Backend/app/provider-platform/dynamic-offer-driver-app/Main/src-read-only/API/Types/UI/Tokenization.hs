{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Tokenization where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Servant
import Tools.Auth

data GetTokenRes = GetTokenRes {expiry :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime, token :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
